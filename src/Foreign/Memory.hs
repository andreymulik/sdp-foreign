module Foreign.Memory
(
  -- * Export
  module Foreign.Marshal,
  
  module Foreign.ForeignPtr,
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


