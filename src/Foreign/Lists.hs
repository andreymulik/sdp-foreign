{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{- |
    Module      :  Foreign.Lists
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Foreign.Lists@ provide foreign linked and doubly linked lists.
-}
module Foreign.Lists
(
  -- * Exports
  module SDP.IndexedM,
  
  -- * Lists
  Linked (..), Linked2 (..)
)
where

import Prelude ()
import SDP.SafePrelude
import SDP.IndexedM hiding ( sizeOf )

import Foreign.Storable
import Foreign.Memory

import Control.Exception.C

default ()

--------------------------------------------------------------------------------

{- Linked section. -}

data LinkedRep e = LinkedRep (Ptr e) (Linked e) deriving ( Eq )

-- | @Linked@ is foreign linked list type for C interfacing.
newtype Linked e = Linked (Ptr (LinkedRep e)) deriving ( Eq )

data LinkedRep2 e = LinkedRep2 (Ptr e) (Linked2 e) (Linked2 e) deriving ( Eq )

-- | @Linked2@ is foreign double linked list type for C interfacing.
newtype Linked2 e = Linked2 (Ptr (LinkedRep2 e)) deriving ( Eq )

--------------------------------------------------------------------------------

{- Nullable and Destruct instances. -}

instance Nullable (Linked e)
  where
    lzero  = Linked Z
    isNull = (== Z)

instance Nullable (Linked2 e)
  where
    lzero  = Linked2 Z
    isNull = (== Z)

instance Destruct Linked
  where
    free Z = return ()
    free (Linked es) = do
      (LinkedRep e' nx) <- get' es
      free (castPtr e')
      free nx
    
    destruct _ Z = return ()
    destruct f (Linked es) = do
      (LinkedRep e' nx) <- get' es
      f (castPtr e')
      destruct f nx

instance Destruct Linked2
  where
    free = go <=< getStart2
      where
        go (Linked2 es) = unless (isNull es) $ do
          (LinkedRep2 e' _ nx) <- get' es
          free (castPtr e')
          go nx
    
    destruct f = go <=< getStart2
      where
        go (Linked2 es) = unless (isNull es) $ do
          (LinkedRep2 e' _ nx) <- get' es
          f (castPtr e')
          go nx

--------------------------------------------------------------------------------

{- Storable instances. -}

instance Storable (Linked e)
  where
    sizeOf  _ = ptrSize
    alignment = sizeOf
    
    peek p = Linked <$> peekByteOff p 0
    poke p = \ (Linked l) -> pokeByteOff p 0 l

instance Storable (LinkedRep e)
  where
    sizeOf  _ = 2 * ptrSize
    alignment = sizeOf
    
    peek p = liftA2 (flip LinkedRep . Linked) (peekByteOff p ptrSize) (peekByteOff p 0)
    poke p = \ (LinkedRep e' (Linked nx)) -> do pokeByteOff p 0 e'; pokeByteOff p ptrSize nx

instance Storable (Linked2 e)
  where
    sizeOf  _ = ptrSize
    alignment = sizeOf
    
    peek p = Linked2 <$> peekByteOff p 0
    poke p = \ (Linked2 l) -> pokeByteOff p 0 l

instance Storable (LinkedRep2 e)
  where
    sizeOf  _ = 3 * ptrSize
    alignment = sizeOf
    
    peek p = do
      e' <- peekByteOff p 0
      pr <- peekByteOff p ptrSize
      nx <- peekByteOff p (2 * ptrSize)
      return $ LinkedRep2 e' (Linked2 pr) (Linked2 nx)
    
    poke p (LinkedRep2 e' (Linked2 pr) (Linked2 nx)) = do
      pokeByteOff p 0 e'
      pokeByteOff p ptrSize pr
      pokeByteOff p (2 * ptrSize) nx

--------------------------------------------------------------------------------

{- BorderedM and LinearM instances. -}

instance BorderedM IO (Linked e) Int
  where
    getLower  _ = return 0
    getSizeOf   = i_foldrM (\ _ c -> return $ c + 1) 0
    getUpper  p = do s <- getSizeOf p; return (s - 1)
    getBounds p = do s <- getSizeOf p; return (0, s - 1)

instance BorderedM IO (Linked2 e) Int
  where
    getLower  _ = return 0
    getUpper  p = do s <- getSizeOf p; return (s - 1)
    getBounds p = do s <- getSizeOf p; return (0, s - 1)
    
    getSizeOf p = do s1 <- pre 0 p; s2 <- nxt 0 p; return (s1 + s2 - 1)
      where
        pre i es = isNull es ? return i $ pre (i + 1) =<< getPrev2 es
        nxt i es = isNull es ? return i $ nxt (i + 1) =<< getNext2 es

instance LinearM IO (Linked e) (Ptr e)
  where
    nowNull es = return (isNull es)
    getHead es = isNull es ? nullEx "getHead" $ getElem es
    getLast es = isNull es ? nullEx "getLast" $ getEnd es >>= getLast
    
    singleM e = Linked <$> new (LinkedRep e Z)
    
    prepend e es = Linked <$> new (LinkedRep e es)
    
    append es e = isNull es ? singleM e $ do
      (Linked end) <- getEnd es
      (Linked  xs) <- singleM e
      pokeByteOff end ptrSize xs
      return es
    
    fromFoldableM = foldr ((=<<) . prepend) (return Z)
    newLinear     = fromFoldableM
    
    getLeft  = i_foldrM (return ... (:)) []
    getRight = i_foldlM (return ... flip (:)) []
    
    (!#>) = (>!)
    
    reversed es = do xs <- i_foldlM (flip prepend) Z es; free es; return xs
    
    filled n e = n < 1 ? return Z $ filled (n - 1) e >>= prepend e
    
    copyTo src os trg ot c = when (c > 0) $ do
        when (os < 0 || ot < 0) $ underEx "copyTo"
        skip' src os >>=<< skip' trg ot $ go c
      where
        go n xs ys@(Linked ys') = when (n > 0) $ do
          when (isNull xs || isNull ys) $ overEx "copyTo"
          
          y' <- peekByteOff ys' 0
          pokeByteOff y' 0 =<< getElem xs
          
          getNext xs >>=<< getNext ys $ go (n - 1)
        
        skip' Z  _ = overEx "copyTo"
        skip' es 0 = return es
        skip' es i = do nx <- getNext es; skip' nx (i - 1)

instance LinearM IO (Linked2 e) (Ptr e)
  where
    nowNull es = return (isNull es)
    getHead es = isNull es ? nullEx "getHead" $ getElem2 es
    getLast es = isNull es ? nullEx "getLast" $ getEnd2 es >>= getElem2
    
    singleM e = Linked2 <$> new (LinkedRep2 e Z Z)
    
    prepend e Z = singleM e
    prepend e es = do
      nx   <- getStart2 es
      node <- new $ LinkedRep2 e Z nx
      pokeByteOff (unpack nx) ptrSize node
      return (Linked2 node)
    
    append Z e = Linked2 <$> new (LinkedRep2 e Z Z)
    append es e = do
      pr   <- getEnd2 es
      node <- new (LinkedRep2 e pr Z)
      pokeByteOff (unpack pr) (2 * ptrSize) node
      return (Linked2 node)
    
    newLinear     = fromFoldableM
    fromFoldableM = foldr ((=<<) . prepend) (return Z)
    
    getLeft = go <=< getStart2
      where
        go Z  = return []
        go es = liftA2 (flip (:)) (go =<< getNext2 es) (getElem2 es)
    
    getRight = go <=< getEnd2
      where
        go Z  = return []
        go es = liftA2 (flip (:)) (go =<< getPrev2 es) (getElem2 es)
    
    (!#>) = (>!)
    
    reversed es = do getStart2 es >>= go; return es
      where
        go (Linked2 xs) = unless (isNull xs) $ do
          (LinkedRep2 _ (Linked2 pr) (Linked2 nx)) <- peek xs
          pokeByteOff xs ptrSize nx
          pokeByteOff xs (2 * ptrSize) pr
          go (Linked2 nx)
    
    filled n e = n < 1 ? return Z $ filled (n - 1) e >>= prepend e
    
    copyTo src os trg ot c = when (c > 0) $ do
        when (os < 0 || ot < 0) $ underEx "copyTo"
        (getStart2 src >>= skip' os) >>=<< (getStart2 trg >>= skip' ot) $ go c
      where
        skip' _ Z  = overEx "copyTo"
        skip' 0 es = return es
        skip' i es = getNext2 es >>= skip' (i - 1)
        
        go n xs ys@(Linked2 ys') = when (n > 0) $ do
          when (isNull xs || isNull ys) $ overEx "copyTo"
          y' <- peekByteOff ys' 0
          pokeByteOff y' 0 =<< getElem2 xs
          getNext2 xs >>=<< getNext2 ys $ go (n - 1)

--------------------------------------------------------------------------------

{- IndexedM and IFoldM instances. -}

instance IndexedM IO (Linked e) Int (Ptr e)
  where
    fromAssocs' bnds = newLinear ... assoc' bnds
    
    fromIndexed' = newLinear . listL
    fromIndexedM = newLinear <=< getLeft
    
    es >! i = i == 0 ? getElem es $ do nx <- getNext es; nx !> (i - 1)
    
    es !> i = isNull es ? overEx "!>" $ case i <=> 0 of
      GT -> do nx <- getNext es; nx !> (i - 1)
      LT -> undEx "(!>) {Linked}"
      EQ -> getElem es
    
    overwrite xs ascs = isNull xs ? return xs $ do
      bnds@(l, _) <- getBounds xs
      sequence_ [ writeM_ xs (i - l) e | (i, e) <- ascs, inRange bnds i ]
      return xs
    
    writeM_ (Linked es) 0 e = unless (isNull es) $ do
      x <- peekByteOff es 0
      free (x `asTypeOf` e)
      pokeByteOff es 0 e
    writeM_ es i e = unless (isNull es || i < 0) $ do
      nx <- getNext es
      writeM_ nx (i - 1) e

instance IndexedM IO (Linked2 e) Int (Ptr e)
  where
    fromAssocs' bnds = newLinear ... assoc' bnds
    
    fromIndexed' = newLinear . listL
    fromIndexedM = newLinear <=< getLeft
    
    overwrite es ascs = go =<< getStart2 es
      where
        go (Linked2 Z) = return Z
        go xs = do
          bnds@(l, _) <- getBounds xs
          sequence_ [ writeM_ xs (i - l) e | (i, e) <- ascs, inRange bnds i ]
          return xs
    
    writeM_ (Linked2  Z) _ _ = return ()
    writeM_ (Linked2 es) 0 e = do
      x <- peekByteOff es 0
      free (x `asTypeOf` e)
      pokeByteOff es 0 e
    writeM_ es i e = do nx <- getNext2 es; writeM_ nx i e
    
    es >! i =
      let go o = o == 0 ? getElem2 $ go (o - 1) <=< getNext2
      in  go i =<< getStart2 es
    
    es !> i = do
        when (isNull es) $ nullEx "(!>) {Linked2}"
        when   (i < 0)   $ undEx  "(!>) {Linked2}"
        go i =<< getStart2 es
      where
        go _ Z  = overEx "(!>) {Linked2}"
        go 0 xs = getElem2 xs
        go o xs = go (o - 1) =<< getNext2 xs

instance IFoldM IO (Linked e) Int (Ptr e)
  where
    i_foldrM _ base Z  = return base
    i_foldrM f base es = (i_foldrM f base =<< getNext es) >>=<< getElem es $ flip f
    
    i_foldlM _ base Z  = return base
    i_foldlM f base es = (f base =<< getElem es) >>=<< getNext es $ i_foldlM f
    
    ofoldrM = ifoldrM
    ofoldlM = ifoldlM
    
    ifoldrM = go 0
      where
        go _ _ base Z = return base
        go o f base es = (go (o + 1) f base =<< getNext es) >>=<< getElem es $ flip (f o)
    
    ifoldlM = go 0
      where
        go _ _ base Z = return base
        go o f base es = (f o base =<< getElem es) >>=<< getNext es $ go (o + 1) f

instance IFoldM IO (Linked2 e) Int (Ptr e)
  where
    i_foldrM g b xs = go g b =<< getStart2 xs
      where
        go _ base Z  = return base
        go f base es = (getNext2 es >>= go f base) >>=<< getElem2 es $ flip f
    
    i_foldlM g b xs = go g b =<< getStart2 xs
      where
        go _ base Z  = return base
        go f base es = (getElem2 es >>= f base) >>=<< getNext2 es $ go f
    
    ifoldrM g b xs = go 0 g b =<< getStart2 xs
      where
        go _ _ base Z  = return base
        go o f base es = (getNext2 es >>= go (o + 1) f base) >>=<< getElem2 es $ flip (f o)
    
    ifoldlM g b xs = go 0 g b =<< getStart2 xs
      where
        go _ _ base Z  = return base
        go o f base es = (getElem2 es >>= f o base) >>=<< getNext2 es $ go (o + 1) f

--------------------------------------------------------------------------------

get' :: (Storable e) => Ptr e -> IO e
get' p = do e <- peek p; free p; return e

getElem :: Linked e -> IO (Ptr e)
getElem (Linked es) = peekByteOff es 0

getNext :: Linked e -> IO (Linked e)
getNext (Linked es) = Linked <$> peekByteOff es ptrSize

getEnd :: Linked e -> IO (Linked e)
getEnd es = do nx <- getNext es; isNull nx ? return es $ getEnd nx

{-# INLINE unpack #-}
unpack :: Linked2 e -> Ptr (LinkedRep2 e)
unpack =  \ (Linked2 e) -> e

getElem2 :: Linked2 e -> IO (Ptr e)
getElem2 Z = nullEx "(>!)"
getElem2 (Linked2 es) = peekByteOff es 0

getPrev2 :: Linked2 e -> IO (Linked2 e)
getPrev2 Z = nullEx "getPrev2"
getPrev2 (Linked2 es) = Linked2 <$> peekByteOff es ptrSize

getNext2 :: Linked2 e -> IO (Linked2 e)
getNext2 Z = nullEx "getNext2"
getNext2 (Linked2 es) = Linked2 <$> peekByteOff es (2 * ptrSize)

getStart2 :: Linked2 e -> IO (Linked2 e)
getStart2 Z  = return Z
getStart2 es = do pr <- getPrev2 es; isNull pr ? return es $ getStart2 pr

getEnd2 :: Linked2 e -> IO (Linked2 e)
getEnd2 Z  = return Z
getEnd2 es = do nx' <- getNext2 es; isNull nx' ? return es $ getEnd2 nx'

undEx :: String -> IO a
undEx =  throwIO . IndexUnderflow . showString "in Foreign.Lists."

nullEx :: String -> IO a
nullEx =  throwIO . NullPointerException . showString "in Foreign.Lists."

overEx :: String -> IO a
overEx =  throwIO . IndexOverflow . showString "in Foreign.Lists."

underEx :: String -> IO a
underEx =  throwIO . IndexUnderflow . showString "in Foreign.Lists."

