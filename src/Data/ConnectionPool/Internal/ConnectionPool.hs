{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       $HEADER$
-- Description:  ConnectionPool data type which is a specialized Pool wrapper.
-- Copyright:    (c) 2014-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  DeriveDataTypeable, FlexibleContexts, NamedFieldPuns,
--               NoImplicitPrelude, RecordWildCards
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
    ( ConnectionPool(ConnectionPool, _handlerParams, _resourcePool)
    , resourcePool
    , handlerParams
    , createConnectionPool
    , destroyAllConnections
    , withConnection
    )
  where

import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import Data.Tuple (fst, uncurry)
import Data.Typeable (Typeable)
import System.IO (IO)
import Text.Show (Show(showsPrec), showChar, shows, showString)

import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Socket (Socket)

import Data.Function.Between.Strict ((~@@^>))
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
data ConnectionPool handlerParams a = ConnectionPool
    { _resourcePool :: !(Pool (Socket, a))
    , _handlerParams :: !handlerParams
    }
  deriving (Typeable)

-- | @since 0.1.3
instance Show handlerParams => Show (ConnectionPool handlerParams a) where
    showsPrec _ ConnectionPool{..} =
        showString "ConnectionPool {resourcePool = " . shows _resourcePool
        . showString ", handlerParams = " . shows _handlerParams . showChar '}'

resourcePool
    :: Functor f
    => ((Pool (Socket, a)) -> f (Pool (Socket, b)))
    -> ConnectionPool handlerParams a -> f (ConnectionPool handlerParams b)
resourcePool = _resourcePool ~@@^> \s b -> s{_resourcePool = b}

handlerParams
    :: Functor f
    => (handlerParams -> f handlerParams')
    -> ConnectionPool handlerParams c -> f (ConnectionPool handlerParams' c)
handlerParams = _handlerParams ~@@^> \s b -> s{_handlerParams = b}

-- | Specialized wrapper for 'Pool.createPool', see its documentation for
-- details.
createConnectionPool
    :: handlerParams
    -- ^ Data type passed down to individual connection handlers.
    -> IO (Socket, a)
    -- ^ Acquire a connection which is represented by a 'Socket'. There might
    -- be additional information associated with specific connection that we
    -- pass as a sencond value in a tuple. Such information are considered read
    -- only and aren't passed to release function (see next argument).
    -> (Socket -> IO ())
    -- ^ Release a connection which is represented by a 'Socket'.
    -> ResourcePoolParams
    -- ^ Data type representing all 'Pool.createPool' parameters that describe
    -- internal 'Pool' parameters.
    -> IO (ConnectionPool handlerParams a)
    -- ^ Created connection pool that is parametrised by additional connection
    -- details.
createConnectionPool hParams acquire release params =
    mkConnectionPool <$> Pool.createPool
        acquire
        (release . fst)
        (ResourcePoolParams._numberOfStripes params)
        (ResourcePoolParams._resourceIdleTimeout params)
        (ResourcePoolParams._numberOfResourcesPerStripe params)
  where
    mkConnectionPool pool = ConnectionPool
        { _resourcePool = pool
        , _handlerParams = hParams
        }

-- | Specialized wrapper for 'Pool.withResource'.
withConnection
    :: MonadBaseControl IO m
    => ConnectionPool c a
    -> (c -> Socket -> a -> m r)
    -> m r
withConnection ConnectionPool{..} f =
    Pool.withResource _resourcePool (uncurry (f _handlerParams))

-- | Destroy all connections that might be still open in a connection pool.
-- This is useful when one needs to release all resources at once and not to
-- wait for idle timeout to be reached.
--
-- For more details see 'Pool.destroyAllResources'.
--
-- /Since version 0.1.1.0./
destroyAllConnections :: ConnectionPool handlerParams a -> IO ()
destroyAllConnections ConnectionPool{_resourcePool} =
    Pool.destroyAllResources _resourcePool
