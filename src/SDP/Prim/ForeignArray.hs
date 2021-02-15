{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, MagicHash #-}

{- |
    Module      :  SDP.Prim.ForeignArray
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @SDP.Prim.ForeignArray@ is internal module, that represent strict unboxed
    'ForeignPtr'-based array type 'ForeignArray'.
-}
module SDP.Prim.ForeignArray
(
  -- * Exports
  module Foreign.Memory,
  module SDP.IndexedM,
  module SDP.SortM,
  
  -- * Pointer array
  ForeignArray
)
where

import Prelude ()
import SDP.IndexedM
import SDP.SafePrelude

import SDP.Prim.PtrArray hiding ( void )
import SDP.Prim.SArray
import SDP.Prim.SBytes

import SDP.SortM.Tim
import SDP.SortM

import Foreign.Storable hiding ( sizeOf )
import Foreign.Memory   hiding (  void  )

import Data.Typeable

import Control.Exception.SDP

default ()

--------------------------------------------------------------------------------

-- | 'ForeignArray' - pseudo-primitive strict unboxed mutable array.
data ForeignArray e = ForeignArray
    {-# UNPACK #-} !Int -- ^ Element count (not a real size)
    {-# UNPACK #-} !Int -- ^ Offset (is elements)
    !(ForeignPtr e)     -- ^ Real pointer
  deriving ( Eq, Typeable )

--------------------------------------------------------------------------------

{- Nullable, Estimate, Bordered and BorderedM instances. -}

instance Nullable (ForeignArray e)
  where
    isNull (ForeignArray c _ ptr) = c < 1 || isNull ptr
    
    lzero = ForeignArray 0 0 Z

instance Estimate (ForeignArray e)
  where
    (<==>) = on (<=>) sizeOf
    (.>=.) = on (>=)  sizeOf
    (.<=.) = on (<=)  sizeOf
    (.>.)  = on (>)   sizeOf
    (.<.)  = on (<)   sizeOf
    
    (<.=>) = (<=>) . sizeOf
    (.>=)  = (>=)  . sizeOf
    (.<=)  = (<=)  . sizeOf
    (.>)   = (>)   . sizeOf
    (.<)   = (<)   . sizeOf

instance Bordered (ForeignArray e) Int
  where
    bounds (ForeignArray c _ _) = (0, c - 1)
    sizeOf (ForeignArray c _ _) = c

instance BorderedM IO (ForeignArray e) Int
  where
    nowIndexIn (ForeignArray c _ _) = return . inRange (0, c - 1)
    getIndices (ForeignArray c _ _) = return [0 .. c - 1]
    getBounds  (ForeignArray c _ _) = return (0, c - 1)
    getUpper   (ForeignArray c _ _) = return (c - 1)
    getSizeOf  (ForeignArray c _ _) = return c
    
    getLower _ = return 0

--------------------------------------------------------------------------------

{- LinearM and SplitM instances. -}

instance (Storable e) => LinearM IO (ForeignArray e) e
  where
    newNull = return Z
    nowNull = return . isNull
    
    getHead es = es >! 0
    getLast es = es >! upper es
    
    newLinear es = ForeignArray (length es) 0 <$> toForeign (newArray es)
    
    newLinearN n es = ForeignArray n 0 <$> toForeign (newArray es)
    
    getLeft (ForeignArray n _ ptr) = ptr `withForeignPtr` peekArray n
    
    {-# INLINE (!#>) #-}
    ForeignArray _ o ptr !#> i = ptr `withForeignPtr` flip peekElemOff (o + i)
    
    {-# INLINE writeM #-}
    writeM (ForeignArray _ o ptr') i e = ptr' `withForeignPtr` \ ptr ->
      pokeElemOff ptr (o + i) e
    
    copied (ForeignArray n o ptr) = do
      copy <- mallocArray n
      copyArray' copy (ptr +& o) n
      ForeignArray n 0 <$> newForeignPtr_ copy
    
    copied' (ForeignArray n o ptr) l c = do
      let n' = max 0 (min n c)
      copy <- mallocArray n'
      copyArray' copy (ptr +& (o + l)) n'
      ForeignArray n' o <$> newForeignPtr_ copy
    
    filled n e = do
      let n' = max 0 n
      ptr <- mallocArray n'
      forM_ [0 .. n' - 1] $ \ i -> pokeElemOff ptr i e
      ForeignArray n' 0 <$> newForeignPtr_ ptr
    
    copyTo (ForeignArray n1 o1 src) sc (ForeignArray n2 o2 trg) tc n = when (n > 0) $ do
      when      (sc < 0 || tc < 0)      $ underEx "copyTo"
      when (sc + n > n1 || tc + n > n2) $ overEx  "copyTo"
      copyArray'' (trg +& (o1 + sc)) (src +& (o2 + tc)) n
    
    merged ess = do
        ptr <- mallocArray n
        let writer (ForeignArray c o arr) i = i + c <$ copyArray' ptr (arr +& (o + i)) c
        void $ foldr ((=<<) . writer) (return 0) ess
        ForeignArray n 0 <$> newForeignPtr_ ptr
      where
        n = foldr' ((+) . sizeOf) 0 ess
    
    ofoldrM f base = \ arr@(ForeignArray n _ _) ->
      let go i =  n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f i
      in  go 0
    
    ofoldlM f base = \ arr@(ForeignArray n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f i
      in  go (n - 1)
    
    foldrM f base = \ arr@(ForeignArray n _ _) ->
      let go i = n == i ? return base $ (arr !#> i) >>=<< go (i + 1) $ f
      in  go 0
    
    foldlM f base = \ arr@(ForeignArray n _ _) ->
      let go i = -1 == i ? return base $ go (i - 1) >>=<< (arr !#> i) $ f
      in  go (n - 1)

instance (Storable e) => SplitM IO (ForeignArray e) e
  where
    takeM n es@(ForeignArray c o ptr)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (ForeignArray n o ptr)
    
    dropM n es@(ForeignArray c o ptr)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (ForeignArray (c - n) (o + n) ptr)
    
    keepM n es@(ForeignArray c o ptr)
      | n <= 0 = newNull
      | n >= c = return es
      |  True  = return (ForeignArray n (c - n + o) ptr)
    
    sansM n es@(ForeignArray c o ptr)
      | n >= c = newNull
      | n <= 0 = return es
      |  True  = return (ForeignArray (c - n) o ptr)
    
    splitM n es@(ForeignArray c o ptr)
      | n <= 0 = do e' <- newNull; return (e', es)
      | n >= c = do e' <- newNull; return (es, e')
      |  True  = return (ForeignArray n o ptr, ForeignArray (c - n) (o + n) ptr)
    
    divideM n es@(ForeignArray c o ptr)
      | n <= 0 = do e' <- newNull; return (es, e')
      | n >= c = do e' <- newNull; return (e', es)
      |  True  = return (ForeignArray n (c - n + o) ptr, ForeignArray (c - n) o ptr)
    
    prefixM p es@(ForeignArray c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ? go (i + 1) $ return i
      in  go 0
    
    suffixM p es@(ForeignArray c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ? go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))
    
    mprefix p es@(ForeignArray c _ _) =
      let go i = i >= c ? return i $ do e <- es !#> i; p e ?^ go (i + 1) $ return i
      in  go 0
    
    msuffix p es@(ForeignArray c _ _) =
      let go i = i == 0 ? return c $ do e <- es !#> i; p e ?^ go (i - 1) $ return (c - i - 1)
      in  go (max 0 (c - 1))

--------------------------------------------------------------------------------

{- IndexedM and SortM instances. -}

instance (Storable e) => MapM IO (ForeignArray e) Int e
  where
    newMap ascs = do
      let n = sizeOf ascs
      es <- ForeignArray n 0 <$> toForeign (callocArray n)
      overwrite es ascs
    
    newMap' defvalue ascs = sizeOf ascs `filled` defvalue >>= (`overwrite` ascs)
    
    (>!) = (!#>)
    
    overwrite es ascs =
      let ies = filter (indexIn es . fst) ascs
      in  mapM_ (uncurry $ writeM es) ies >> return es
    
    kfoldrM = ofoldrM
    kfoldlM = ofoldlM

instance (Storable e) => IndexedM IO (ForeignArray e) Int e
  where
    fromAssocs bnds ascs = do
      let n = size bnds
      es <- ForeignArray n 0 <$> toForeign (callocArray n)
      overwrite es ascs
    
    fromAssocs' bnds defvalue ascs = size bnds `filled` defvalue >>= (`overwrite` ascs)
    
    {-# INLINE writeM' #-}
    writeM' es = \ i -> when (indexIn es i) . writeM es i
    
    fromIndexed' es = do
      let n = sizeOf es
      copy <- ForeignArray n 0 <$> toForeign (mallocArray n)
      forM_ [0 .. n - 1] $ \ i -> writeM copy i (es !^ i)
      return copy
    
    fromIndexedM es = do
      n    <- getSizeOf es
      copy <- ForeignArray n 0 <$> toForeign (mallocArray n)
      forM_ [0 .. n - 1] $ \ i -> es !#> i >>= writeM copy i
      return copy

instance (Storable e) => SortM IO (ForeignArray e) e
  where
    sortedMBy f es@(ForeignArray n _ _) = n < 2 ? return True $ go 1 =<< getHead es
      where
        go i e1 = i == n ? return True $ do
          e2 <- es !#> i;
          e1 `f` e2 ? go (i + 1) e2 $ return False
    
    sortMBy = timSortBy

--------------------------------------------------------------------------------

instance IsPtr ForeignArray
  where
    ForeignArray n o ptr +* i = ForeignArray (max 0 (n - i)) (o + i) (castPtr ptr)
    ForeignArray n o ptr +& i = ForeignArray (max 0 (n - i)) (o + i) ptr
    
    castPtr (ForeignArray n o ptr) = ForeignArray n o (castPtr ptr)

instance Destruct ForeignArray
  where
    free (ForeignArray _ _ ptr) = free ptr
    
    destruct f (ForeignArray _ _ ptr) = destruct f ptr

instance Finalize ForeignArray
  where
    finalize (ForeignArray _ _ ptr) = finalize ptr
    touch    (ForeignArray _ _ ptr) = touch    ptr

--------------------------------------------------------------------------------

{- Thaw instances. -}

instance (Storable e) => Thaw IO (SArray# e) (ForeignArray e)
  where
    thaw es = do
      let n = sizeOf es
      ptr <- mallocArray n
      forM_ [0 .. n - 1] $ \ i -> pokeElemOff ptr i (es !^ i)
      ForeignArray n 0 <$> newForeignPtr_ ptr

instance (Unboxed e, Storable e) => Thaw IO (SBytes# e) (ForeignArray e)
  where
    thaw es = do
      let n = sizeOf es
      ptr <- mallocArray n
      forM_ [0 .. n - 1] $ \ i -> pokeElemOff ptr i (es !^ i)
      ForeignArray n 0 <$> newForeignPtr_ ptr

--------------------------------------------------------------------------------

copyArray' :: (Storable e) => Ptr e -> ForeignPtr e -> Int -> IO ()
copyArray' px py' n = py' `withForeignPtr` \ py -> copyArray px py n

copyArray'' :: (Storable e) => ForeignPtr e -> ForeignPtr e -> Int -> IO ()
copyArray'' px' py' n = px' `withForeignPtr` \ px -> copyArray' px py' n

toForeign :: (Storable e) => IO (Ptr e) -> IO (ForeignPtr e)
toForeign =  (>>= newForeignPtr_)

underEx :: String -> a
underEx =  throw . IndexUnderflow . showString "in SDP.Prim.ForeignArray."

overEx :: String -> a
overEx =  throw . IndexOverflow . showString "in SDP.Prim.ForeignArray."


