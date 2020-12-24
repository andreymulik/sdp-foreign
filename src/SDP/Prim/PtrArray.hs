{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash #-}

{- |
    Module      :  SDP.Prim.PtrArray
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Prim.PtrArray@ is internal module, that represent strict unboxed
    'Ptr'-based array type 'PtrArray'.
-}
module SDP.Prim.PtrArray
(
  -- * Exports
  module Foreign.Memory,
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Pointer array
  PtrArray
)
where

import Prelude ()
import SDP.IndexedM
import SDP.SafePrelude

import SDP.Prim.SArray
import SDP.Prim.SBytes

import SDP.SortM.Tim
import SDP.SortM

import Foreign.Storable hiding ( sizeOf )
import Foreign.Marshal  hiding (  free  )
import Foreign.Memory

import Data.Typeable

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'PtrArray' - pseudo-primitive strict unboxed mutable array.
data PtrArray e = PtrArray
    {-# UNPACK #-} !Int -- ^ Element count (not a real size)
    {-# UNPACK #-} !Int -- ^ Offset (is elements)
    !(Ptr e)            -- ^ Real pointer
  deriving ( Eq, Typeable )

--------------------------------------------------------------------------------

{- Nullable, Estimate, Bordered and BorderedM instances. -}

instance Nullable (PtrArray e)
  where
    isNull (PtrArray c _ ptr) = c < 1 || isNull ptr
    
    lzero = PtrArray 0 0 Z

instance Estimate (PtrArray e)
  where
    (<==>) = on (<=>) sizeOf
    
    (.>.)  = on (>)  sizeOf
    (.<.)  = on (<)  sizeOf
    (.<=.) = on (<=) sizeOf
    (.>=.) = on (>=) sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf

instance Bordered (PtrArray e) Int
  where
    bounds (PtrArray c _ _) = (0, c - 1)
    sizeOf (PtrArray c _ _) = c

instance BorderedM IO (PtrArray e) Int
  where
    nowIndexIn (PtrArray c _ _) = return . inRange (0, c - 1)
    getIndices (PtrArray c _ _) = return [0 .. c - 1]
    getBounds  (PtrArray c _ _) = return (0, c - 1)
    getUpper   (PtrArray c _ _) = return (c - 1)
    getSizeOf  (PtrArray c _ _) = return c
    
    getLower _ = return 0

--------------------------------------------------------------------------------

{- LinearM and SplitM instances. -}

instance (Storable e) => LinearM IO (PtrArray e) e
  where
    newNull = return Z
    nowNull = return . isNull
    
    getHead es = es >! 0
    getLast es = es >! upper es
    
    newLinear es = PtrArray (length es) 0 <$> newArray es
    
    newLinearN n es = PtrArray n 0 <$> newArray es
    
    getLeft (PtrArray n _ ptr) = peekArray n ptr
    
    {-# INLINE (!#>) #-}
    (!#>) (PtrArray _ o ptr) i = peekElemOff ptr (o + i)
    
    {-# INLINE writeM #-}
    writeM (PtrArray _ o ptr) = pokeElemOff ptr . (o +)
    
    copied (PtrArray n o ptr) = do
      copy <- mallocArray n
      copyArray copy (ptr *& o) n
      return (PtrArray n 0 copy)
    
    copied' (PtrArray n o ptr) l c = do
      let n' = max 0 (min n c)
      copy <- mallocArray n'
      copyArray copy (ptr *& (o + l)) n'
      return (PtrArray n' o ptr)
    
    filled n e = do
      let n' = max 0 n
      ptr <- mallocArray n'
      forM_ [0 .. n' - 1] $ \ i -> pokeElemOff ptr i e
      return (PtrArray n' 0 ptr)
    
    copyTo (PtrArray n1 o1 src) sc (PtrArray n2 o2 trg) tc n = when (n > 0) $ do
      when      (sc < 0 || tc < 0)      $ underEx "copyTo"
      when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
      copyArray (trg *& (o1 + sc)) (src *& (o2 + tc)) n
    
    merged ess = do
        ptr <- mallocArray n
        let writer (PtrArray c o arr') i = (i + c) <$ copyArray ptr (arr' *& (o + i)) c
        PtrArray n 0 ptr <$ foldr ((=<<) . writer) (return 0) ess
      where
        n = foldr' ((+) . sizeOf) 0 ess
    
    ofoldrM f base = \ arr@(PtrArray n _ _) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM f base = \ arr@(PtrArray n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
      in  go (n - 1)
    
    o_foldrM f base = \ arr@(PtrArray n _ _) ->
      let go i = n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    o_foldlM f base = \ arr@(PtrArray n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (n - 1)

instance (Storable e) => SplitM IO (PtrArray e) e
  where
    takeM n es@(PtrArray c o ptr)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (PtrArray n o ptr)
    
    dropM n es@(PtrArray c o ptr)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (PtrArray (c - n) (o + n) ptr)
    
    keepM n es@(PtrArray c o ptr)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (PtrArray n (c - n + o) ptr)
    
    sansM n es@(PtrArray c o ptr)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (PtrArray (c - n) o ptr)
    
    splitM n es@(PtrArray c o ptr)
      | n <= 0 = do e' <- newNull; return (e', es)
      | n >= c = do e' <- newNull; return (es, e')
      |  True  = return (PtrArray n o ptr, PtrArray (c - n) (o + n) ptr)
    
    divideM n es@(PtrArray c o ptr)
      | n <= 0 = do e' <- newNull; return (es, e')
      | n >= c = do e' <- newNull; return (e', es)
      |  True  = return (PtrArray n (c - n + o) ptr, PtrArray (c - n) o ptr)
    
    prefixM p es@(PtrArray c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ? go (i + 1) $ return i
      in  go 0
    
    suffixM p es@(PtrArray c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ? go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))
    
    mprefix p es@(PtrArray c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ?^ go (i + 1) $ return i
      in  go 0
    
    msuffix p es@(PtrArray c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ?^ go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- IndexedM and SortM instances. -}

instance (Storable e) => MapM IO (PtrArray e) Int e
  where
    newMap ascs = do
      let n = sizeOf ascs
      es <- PtrArray n 0 <$> callocArray n
      overwrite es ascs
    
    newMap' defvalue ascs = sizeOf ascs `filled` defvalue >>= (`overwrite` ascs)
    
    (>!) = (!#>)
    
    overwrite es ascs =
      let ies = filter (indexIn es . fst) ascs
      in  mapM_ (uncurry $ writeM es) ies >> return es
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance (Storable e) => IndexedM IO (PtrArray e) Int e
  where
    fromAssocs bnds ascs = do
      let n = size bnds
      es <- PtrArray n 0 <$> callocArray n
      overwrite es ascs
    
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE writeM' #-}
    writeM' es = \ i -> when (indexIn es i) . writeM es i
    
    fromIndexed' es = do
      let n = sizeOf es
      copy <- PtrArray n 0 <$> mallocArray n
      forM_ [0 .. n - 1] $ \ i -> writeM copy i (es !^ i)
      return copy
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- PtrArray n 0 <$> mallocArray n
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM copy i
      return copy

instance (Storable e) => SortM IO (PtrArray e) e where sortMBy = timSortBy

--------------------------------------------------------------------------------

instance IsPtr PtrArray
  where
    (PtrArray n o ptr) +* i = PtrArray (max 0 (n - i)) (o + i) (castPtr ptr)
    (PtrArray n o ptr) +& i = PtrArray (max 0 (n - i)) (o + i) ptr
    
    castPtr (PtrArray n o ptr) = PtrArray n o (castPtr ptr)

instance Destruct PtrArray
  where
    destruct f (PtrArray _ _ ptr) = f ptr
    free       (PtrArray _ _ ptr) = free ptr

--------------------------------------------------------------------------------

{- Thaw instances. -}

instance (Storable e) => Thaw IO (SArray# e) (PtrArray e)
  where
    thaw es = do
      let n = sizeOf es
      ptr <- mallocArray n
      forM_ [0 .. n - 1] $ \ i -> pokeElemOff ptr i (es !^ i)
      return (PtrArray n 0 ptr)

instance (Unboxed e, Storable e) => Thaw IO (SBytes# e) (PtrArray e)
  where
    thaw es = do
      let n = sizeOf es
      ptr <- mallocArray n
      forM_ [0 .. n - 1] $ \ i -> pokeElemOff ptr i (es !^ i)
      return (PtrArray n 0 ptr)

--------------------------------------------------------------------------------

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.PtrArray."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.PtrArray."

