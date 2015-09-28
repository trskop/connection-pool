{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Portability:  DeriveDataTypeable, DeriveGeneric, FunctionalDependencies,
--               FlexibleContexts, MultiParamTypeClasses, NamedFieldPuns,
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
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons> and
-- other non-HaskellPlatform packages with exception of two packages
-- <http://hackage.haskell.org/package/resource-pool resource-pool> and
-- <http://hackage.haskell.org/package/between between>. Another notable thing
-- is that this package is not OS specific. Please, bear this in mind when
-- doing modifications.
module Data.ConnectionPool.Internal.ConnectionPool
    (
    -- * Data Type For Building Connection Pools
      ConnectionPool(ConnectionPool, _handlerParams, _resourcePool)
    -- ** Lenses
    , resourcePool
    , handlerParams
    , HasConnectionPool(connectionPool)

    -- * Lifted Resource Pool Operations
    --
    -- | Operations on 'Pool' lifted to work on 'ConnectionPool' data type.
    , createConnectionPool
    , destroyAllConnections
    , withConnection
    , tryWithConnection
    )
  where

import Data.Function ((.))
import Data.Functor (Functor, (<$>))
import Data.Maybe (Maybe)
import Data.Tuple (fst, uncurry)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.IO (IO)
import Text.Show (Show(showsPrec), showChar, shows, showString)

import Control.Monad.Trans.Control (MonadBaseControl)

import Data.Function.Between.Strict ((~@@^>))
import Data.Pool (Pool)
import qualified Data.Pool as Pool
    ( createPool
    , destroyAllResources
    , tryWithResource
    , withResource
    )

import Data.ConnectionPool.Internal.ResourcePoolParams (ResourcePoolParams)
import qualified Data.ConnectionPool.Internal.ResourcePoolParams
  as ResourcePoolParams (ResourcePoolParams(..))


-- | Simple specialized wrapper for 'Pool'.
--
-- /Definition changed in version 0.1.3 and 0.1.4./
-- /Instance for 'Generic' introduced in version 0.1.4./
data ConnectionPool handlerParams connection connectionInfo = ConnectionPool
    { _resourcePool :: !(Pool (connection, connectionInfo))
    -- ^ See 'resourcePool' for details.
    --
    -- /Since version 0.1.3; changed in 0.1.4./
    , _handlerParams :: !handlerParams
    -- ^ See 'handlerParams' for details.
    --
    -- /Since version 0.1.3./
    }
  deriving (Generic, Typeable)

-- | /Since version 0.1.3./
instance Show handlerParams => Show (ConnectionPool handlerParams c i) where
    showsPrec _ ConnectionPool{..} =
        showString "ConnectionPool {resourcePool = " . shows _resourcePool
        . showString ", handlerParams = " . shows _handlerParams . showChar '}'

-- | Lens for accessing underlying resource pool @'Pool' (connection,
-- connectionInfo)@. Where @connection@ represents network connection and
-- @connectionInfo@ is a protocol specific information associated with the same
-- network connection as the @connection@ is.
--
-- /Since version 0.1.3; changed in 0.1.4./
resourcePool
    :: Functor f
    => (Pool (c, i) -> f (Pool (c', i')))
    -> ConnectionPool p c i -> f (ConnectionPool p c' i')
resourcePool = _resourcePool ~@@^> \s b -> s{_resourcePool = b}
{-# INLINE resourcePool #-}

-- | Lens for accessing parameters passed down to connection handler. These
-- information will usually be implementation specific. E.g. for
-- <https://hackage.haskell.org/package/streaming-commons streaming-commons> >=
-- 1.13 we use this to pass around read buffer size, for more details see
-- module "Data.ConnectionPool.Internal.HandlerParams".
--
-- /Since version 0.1.3./
handlerParams
    :: Functor f
    => (handlerParams -> f handlerParams')
    -> ConnectionPool handlerParams c i
    -> f (ConnectionPool handlerParams' c i)
handlerParams = _handlerParams ~@@^> \s b -> s{_handlerParams = b}
{-# INLINE handlerParams #-}

-- | Specialized wrapper for 'Pool.createPool', see its documentation for
-- details.
--
-- Definition changed in /version 0.1.3 and version 0.1.4/.
createConnectionPool
    :: handlerParams
    -- ^ Data type passed down to individual connection handlers.
    --
    -- /Since version 0.1.3./
    -> IO (connection, connectionInfo)
    -- ^ Acquire a connection which is represented by a @connection@. There
    -- might be additional information associated with specific connection that
    -- we pass as a sencond value in a tuple. Such information are considered
    -- read only and aren't passed to release function (see next argument).
    --
    -- /Changed in version 0.1.4./
    -> (connection -> IO ())
    -- ^ Release a connection which is represented by a @connection@.
    --
    -- /Changed in version 0.1.4./
    -> ResourcePoolParams
    -- ^ Data type representing all 'Pool.createPool' parameters that describe
    -- internal 'Pool' parameters.
    -> IO (ConnectionPool handlerParams connection connectionInfo)
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
{-# INLINE createConnectionPool #-}

-- | Specialized wrapper for 'Pool.withResource'.
--
-- /Changed in version 0.1.4./
withConnection
    :: MonadBaseControl IO m
    => ConnectionPool handlerParams connection connectionInfo
    -> (handlerParams -> connection -> connectionInfo -> m r)
    -> m r
withConnection ConnectionPool{..} f =
    Pool.withResource _resourcePool (uncurry (f _handlerParams))
{-# INLINE withConnection #-}

-- | Specialized wrapper for 'Pool.tryWithResource'.
--
-- /Since version 0.1.4./
tryWithConnection
    :: MonadBaseControl IO m
    => ConnectionPool handlerParams connection connectionInfo
    -> (handlerParams -> connection -> connectionInfo -> m r)
    -> m (Maybe r)
tryWithConnection ConnectionPool{..} f =
    Pool.tryWithResource _resourcePool (uncurry (f _handlerParams))
{-# INLINE tryWithConnection #-}

-- | Destroy all connections that might be still open in a connection pool.
-- This is useful when one needs to release all resources at once and not to
-- wait for idle timeout to be reached.
--
-- For more details see 'Pool.destroyAllResources'.
--
-- /Since version 0.1.1.0./
destroyAllConnections :: ConnectionPool p c i -> IO ()
destroyAllConnections ConnectionPool{_resourcePool} =
    Pool.destroyAllResources _resourcePool
{-# INLINE destroyAllConnections #-}

-- | /Since version 0.1.4./
class HasConnectionPool p c i s | s -> p, s -> c, s -> i where
    -- | Lens for accessing 'ConnectionPool' wrapped in a data type.
    connectionPool
        :: Functor f
        => (ConnectionPool p c i -> f (ConnectionPool p c i))
        -> s -> f s
