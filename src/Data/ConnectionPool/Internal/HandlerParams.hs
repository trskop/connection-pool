{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  HandlerParams data type which is passed to individual
--               connection.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  DeriveDataTypeable, DeriveGeneric, NoImplicitPrelude
--
-- Internal packages are here to provide access to internal definitions for
-- library writers, but they should not be used in application code.
--
-- Preferably use qualified import, e.g.:
--
-- > import qualified Data.ConnectionPool.Internal.HandlerParams as Internal
--
-- This module doesn't depend on
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons>
-- and other non-HaskellPlatform packages with notable exception of
-- <http://hackage.haskell.org/package/resource-pool resource-pool>. Another
-- notable thing is that this package is not OS specific. Please, bear this in
-- mind when doing modifications.
--
-- /Since version 0.1.3./
module Data.ConnectionPool.Internal.HandlerParams
    ( HandlerParams(HandlerParams, _readBufferSize)
    , readBufferSize
    )
  where

import Data.Data (Data, Typeable)
import Data.Functor (Functor)
import Data.Int (Int)
import GHC.Generics (Generic)
import Text.Show (Show)

import Data.Default.Class (Default(def))
import Data.Function.Between.Strict ((~@@^>))


data HandlerParams = HandlerParams
    { _readBufferSize :: !Int
    -- ^ See 'readBufferSize' for details.
    }
  deriving (Data, Generic, Show, Typeable)

-- | @
-- 'readBufferSize' = 32768
-- @
--
-- Package streaming-commons < 0.1.13 used value 4096,
-- streaming-commons == 0.1.13 used 32768, which is 8 * 4096,
-- based on: <https://github.com/fpco/streaming-commons/issues/22 issue #22>
instance Default HandlerParams where
    def = HandlerParams
        { _readBufferSize = 32768
        }

-- | Lens for accessing read buffer size that handler should use when reading
-- data from connection.
readBufferSize
    :: Functor f => (Int -> f Int) -> HandlerParams -> f HandlerParams
readBufferSize = _readBufferSize ~@@^> \s b -> s{_readBufferSize = b}
