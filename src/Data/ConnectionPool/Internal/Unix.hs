{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
#ifdef KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Family of connection pools specialized by transport protocol.
-- Copyright:    (c) 2014-2016 Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  GHC specific language extensions.
--
-- Module defines type family of connection pools that is later specialised
-- using type tags (phantom types) to specialize implementation of underlying
-- 'Internal.ConnectionPool' for various protocols.
--
-- Internal packages are here to provide access to internal definitions for
-- library writers, but they should not be used in application code.
--
-- Preferably use qualified import, e.g.:
--
-- > import qualified Data.ConnectionPool.Internal.Unix as Internal
--
-- This package is OS specific, because Windows doesn't support UNIX Sockets.
-- Please, bear this in mind when doing modifications.
--
-- /Module introduced in version 0.2./
module Data.ConnectionPool.Internal.Unix
    ( ConnectionPool(..)
    , UnixClient

    , createUnixClientPool
    , withUnixClientConnection
    , tryWithUnixClientConnection
    , destroyAllUnixClientConnections
    )
  where

import Data.Function ((.), const)
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)
import System.IO (IO)

import Network.Socket (Socket)

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function.Between.Strict ((<^@~))
import Data.Streaming.Network
    ( AppDataUnix
    , ClientSettingsUnix
    , getPath
    , getSocketUnix
    )

import Data.ConnectionPool.Class
    ( ConnectionPoolFor
        ( HandlerData
        , destroyAllConnections
        , tryWithConnection
        , withConnection
        )
    )
import Data.ConnectionPool.Family (ConnectionPool)
import Data.ConnectionPool.Internal.ConnectionPool
    ( HasConnectionPool(connectionPool)
    )
import qualified Data.ConnectionPool.Internal.ConnectionPool as Internal
    ( ConnectionPool
    , createConnectionPool
    , destroyAllConnections
    , tryWithConnection
    , withConnection
    )
import Data.ConnectionPool.Internal.HandlerParams (HandlerParams)
import Data.ConnectionPool.Internal.ResourcePoolParams (ResourcePoolParams)
import qualified Data.ConnectionPool.Internal.Streaming as Internal
    ( close
    , fromClientSettingsUnix
    , runUnixApp
    )


-- | Type tag used to specialize connection pool for UNIX Socket clients.
--
-- /Instance for 'Generic' introduced in version 0.2./
data UnixClient
  deriving (Generic, Typeable)

-- | Connection pool for UNIX Socket clients.
--
-- /Definition changed in version 0.1.3 and 0.2./
-- /Instances for 'Generic' and 'Show' introduced in version 0.2./
newtype instance ConnectionPool UnixClient =
    UnixConnectionPool (Internal.ConnectionPool HandlerParams Socket ())
  deriving (Generic, Show)

-- | /Since version 0.2./
instance HasConnectionPool HandlerParams Socket () (ConnectionPool UnixClient)
  where
    connectionPool = const UnixConnectionPool <^@~ \(UnixConnectionPool a) -> a
    {-# INLINE connectionPool #-}

-- | Defined using:
--
-- @
-- 'withConnection' = 'withUnixClientConnection'
-- 'destroyAllConnections' = 'destroyAllUnixClientConnections'
-- @
--
-- /Since version 0.2./
instance ConnectionPoolFor UnixClient where
    type HandlerData UnixClient = AppDataUnix

    withConnection = withUnixClientConnection
    {-# INLINE withConnection #-}

    tryWithConnection = tryWithUnixClientConnection
    {-# INLINE tryWithConnection #-}

    destroyAllConnections = destroyAllUnixClientConnections
    {-# INLINE destroyAllConnections #-}

-- | Create connection pool for UNIX Sockets clients.
createUnixClientPool
    :: ResourcePoolParams
    -> ClientSettingsUnix
    -> IO (ConnectionPool UnixClient)
createUnixClientPool poolParams unixParams = UnixConnectionPool
    <$> Internal.createConnectionPool handlerParams acquire release poolParams
  where
    acquire = (, ()) <$> getSocketUnix (getPath unixParams)
    release = Internal.close
    handlerParams = Internal.fromClientSettingsUnix unixParams
{-# INLINE createUnixClientPool #-}

-- | Temporarily take a UNIX Sockets connection from a pool, run client with
-- it, and return it to the pool afterwards. For details how connections are
-- allocated see 'Data.Pool.withResource'.
withUnixClientConnection
    :: (MonadBaseControl io m, io ~ IO)
    => ConnectionPool UnixClient
    -> (AppDataUnix -> m r)
    -> m r
withUnixClientConnection (UnixConnectionPool pool) =
    Internal.withConnection pool . Internal.runUnixApp
{-# INLINE withUnixClientConnection #-}

-- | Similar to 'withConnection', but only performs action if a UNIX Sockets
-- connection could be taken from the pool /without blocking./ Otherwise,
-- 'tryWithResource' returns immediately with 'Nothing' (ie. the action
-- function is not called). Conversely, if a connection can be acquired from
-- the pool without blocking, the action is performed and it's result is
-- returned, wrapped in a 'Just'.
--
-- /Since version 0.2./
tryWithUnixClientConnection
    :: (MonadBaseControl io m, io ~ IO)
    => ConnectionPool UnixClient
    -> (AppDataUnix -> m r)
    -> m (Maybe r)
tryWithUnixClientConnection (UnixConnectionPool pool) =
    Internal.tryWithConnection pool . Internal.runUnixApp
{-# INLINE tryWithUnixClientConnection #-}

-- | Destroy all UNIX Sockets connections that might be still open in a
-- connection pool. This is useful when one needs to release all resources at
-- once and not to wait for idle timeout to be reached.
--
-- For more details see 'Pool.destroyAllResources'.
--
-- /Since version 0.1.1.0./
destroyAllUnixClientConnections
    :: ConnectionPool UnixClient
    -> IO ()
destroyAllUnixClientConnections (UnixConnectionPool pool) =
    Internal.destroyAllConnections pool
{-# INLINE destroyAllUnixClientConnections #-}
