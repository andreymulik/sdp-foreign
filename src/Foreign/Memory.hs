{- |
    Module      :  Foreign.Memory
    Copyright   :  (c) Andrey Mulik 2020
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
    
    @Foreign.Memory@ provide classes for types, which use unmanaged memory.
-}
module Foreign.Memory
(
  -- * Export
  module Foreign.ForeignPtr,
  module Foreign.Marshal,
  module Foreign.Ptr,
  
  -- * Pointer operations.
  IsPtr (..), (*&),  ptrSize,
  
  -- ** Memory deallocation.
  Destruct (..),
  
  -- ** Memory finalization
  Finalize (..)
)
where

import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal    hiding ( free )
import Foreign.Ptr        hiding ( castPtr )

import qualified Foreign as F

import Data.Proxy

default ()

--------------------------------------------------------------------------------

-- | Size of 'Ptr', frequently used value.
ptrSize :: Int
ptrSize =  sizeOf (undefined :: Ptr ())

-- | Advances the given address by the given offset in elements.
(*&) :: (Storable a) => Ptr a -> Int -> Ptr a
ptr *& c = ptr +& (c * sizeOf (undefined `asProxyTypeOf` ptr))

--------------------------------------------------------------------------------

-- | 'IsPtr' is class types, which implements pointer casting and arithmetic.
class IsPtr ptr
  where
    {-# MINIMAL ((+*) | (+&)) #-}
    
    -- | Advances the given address by the given offset in bytes.
    (+*) :: ptr a -> Int -> ptr b
    (+*) =  (+&) . castPtr
    
    -- | Strict version of ('+*').
    (+&) :: ptr a -> Int -> ptr a
    (+&) =  (+*)
    
    -- | Casts a pointer from one type to another.
    castPtr :: ptr a -> ptr b
    castPtr =  (+* 0)

instance IsPtr Ptr
  where
    castPtr = F.castPtr
    (+*)    = F.plusPtr

instance IsPtr ForeignPtr
  where
    castPtr = castForeignPtr
    (+*)    = plusForeignPtr

--------------------------------------------------------------------------------

-- | 'Destruct' is class of types, which deals unmanaged memory.
class Destruct mem
  where
    -- | Free all associated memory blocks.
    free :: mem a -> IO ()
    
    -- | Free all associated memory blocks with given function.
    destruct :: (Ptr a -> IO ()) -> mem a -> IO ()

instance Destruct Ptr
  where
    free = F.free
    
    destruct = ($)

instance Destruct ForeignPtr
  where
    free = finalizeForeignPtr
    
    -- | Ignores given function, run 'finalizeForeignPtr'.
    destruct = const finalizeForeignPtr

--------------------------------------------------------------------------------

-- | 'Finalize' is class of 'ForeignPtr'-based types.
class Finalize mem
  where
    -- | Causes associated finalizers to be run immediately.
    finalize :: mem a -> IO ()
    
    {- |
      This function ensures that the foreign object in question is alive at the
      given place in the sequence of IO actions.
    -}
    touch :: mem a -> IO ()

instance Finalize ForeignPtr
  where
    finalize = finalizeForeignPtr
    touch    = touchForeignPtr

