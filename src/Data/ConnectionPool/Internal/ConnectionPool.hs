{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  ConnectionPool data type which is a specialized Pool wrapper.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  non-portable (DeriveDataTypeable, FlexibleContexts,
--               NoImplicitPrelude)
--
-- Internal packages are here to provide access to internal definitions for
-- library writers, but they should not be used in application code.
--
-- Preferably use qualified import, e.g.:
--
-- > import qualified Data.ConnectionPool.Internal.ConnectionPool as Internal
--
-- This module doesn't depend on
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons>
-- and other non-HaskellPlatform packages with notable exception of
-- <http://hackage.haskell.org/package/resource-pool resource-pool>. Another
-- notable thing is that this package is not OS specific. Please, bear this in
-- mind when doing modifications.
module Data.ConnectionPool.Internal.ConnectionPool
    ( ConnectionPool(ConnectionPool)
    , createConnectionPool
    , destroyAllConnections
    , withConnection
    )
  where

import Control.Applicative ((<$>))
import Data.Function ((.))
import Data.Tuple (fst, uncurry)
import Data.Typeable (Typeable)
import System.IO (IO)

import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Socket (Socket)

import Data.Pool (Pool)
import qualified Data.Pool as Pool
    ( createPool
    , destroyAllResources
    , withResource
    )

import Data.ConnectionPool.Internal.ResourcePoolParams (ResourcePoolParams)
import qualified Data.ConnectionPool.Internal.ResourcePoolParams
  as ResourcePoolParams (ResourcePoolParams(..))


-- | Simple specialized wrapper for 'Pool'.
newtype ConnectionPool a = ConnectionPool (Pool (Socket, a))
  deriving (Typeable)

-- | Specialized wrapper for 'Pool.createPool', see its documentation for
-- details.
createConnectionPool
    :: IO (Socket, a)
    -- ^ Acquire a connection which is represented by a 'Socket'. There might
    -- be additional information associated with specific connection that we
    -- pass as a sencond value in a tuple. Such information are considered read
    -- only and aren't passed to release function (see next argument).
    -> (Socket -> IO ())
    -- ^ Release a connection which is represented by a 'Socket'.
    -> ResourcePoolParams
    -- ^ Data type representing all 'Pool.createPool' parameters that describe
    -- internal 'Pool' parameters.
    -> IO (ConnectionPool a)
    -- ^ Created connection pool that is parametrised by additional connection
    -- details.
createConnectionPool acquire release params =
    ConnectionPool <$> Pool.createPool
        acquire
        (release . fst)
        (ResourcePoolParams._numberOfStripes params)
        (ResourcePoolParams._resourceIdleTimeout params)
        (ResourcePoolParams._numberOfResourcesPerStripe params)

-- | Specialized wrapper for 'Pool.withConnection'.
withConnection
    :: MonadBaseControl IO m
    => ConnectionPool a
    -> (Socket -> a -> m r)
    -> m r
withConnection (ConnectionPool pool) f =
    Pool.withResource pool (uncurry f)

-- | Destroy all connections that might be still open in a connection pool.
-- This is useful when one needs to release all resources at once and not to
-- wait for idle timeout to be reached.
--
-- For more details see 'Pool.destroyAllResources'.
--
-- /Since version 0.1.1.0./
destroyAllConnections :: ConnectionPool a -> IO ()
destroyAllConnections (ConnectionPool pool) = Pool.destroyAllResources pool
