{- |
    Module      :  Control.Exception.SDP
    Copyright   :  (c) Andrey Mulik 2019
    License     :  BSD-style
    Maintainer  :  work.a.mulik@gmail.com
    Portability :  portable
  
  @Control.Exception.C@ - service module that provide some useful exceptions.
-}
module Control.Exception.C
(
  -- * Exports
  module Control.Exception.SDP,
  
  -- * Exceptions
  NullPointerException (..)
)
where

import Control.Exception.SDP
import Data.Typeable

default ()

--------------------------------------------------------------------------------

-- | Null pointer exception.
data NullPointerException = NullPointerException String deriving ( Eq, Typeable )

instance Show NullPointerException
  where
    show (NullPointerException msg) = "NULL pointer " ++ msg

instance Exception NullPointerException

