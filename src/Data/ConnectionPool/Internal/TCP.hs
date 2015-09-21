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
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Family of connection pools specialized by transport protocol.
-- Copyright:    (c) 2014-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  CPP, DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
--               MultiParamTypeClasses, NoImplicitPrelude, PolyKinds,
--               StandaloneDeriving, TypeFamilies
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
-- > import qualified Data.ConnectionPool.Internal.TCP as Internal
--
-- /Module introduced in version 0.1.4./
module Data.ConnectionPool.Internal.TCP
    ( ConnectionPool(..)
    , TcpClient

    , createTcpClientPool
    , withTcpClientConnection
    , destroyAllTcpClientConnections
    )
  where

import Data.Function ((.), const)
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Nothing))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)
import System.IO (IO)

import Network.Socket (SockAddr, Socket)
import qualified Network.Socket as Socket (sClose)

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Function.Between.Strict ((<^@~))
import Data.Streaming.Network
    ( AppData
    , ClientSettings
    )

import Data.ConnectionPool.Class (ConnectionPoolFor(..))
import Data.ConnectionPool.Family (ConnectionPool)
import Data.ConnectionPool.Internal.ConnectionPool
    ( HasConnectionPool(connectionPool)
    )
import qualified Data.ConnectionPool.Internal.ConnectionPool as Internal
    ( ConnectionPool
    , createConnectionPool
    , withConnection
    , destroyAllConnections
    )
import Data.ConnectionPool.Internal.HandlerParams (HandlerParams)
import qualified Data.ConnectionPool.Internal.Streaming as Internal
    ( acquireTcpClientConnection
    , fromClientSettings
    , runTcpApp
    )
import Data.ConnectionPool.Internal.ResourcePoolParams (ResourcePoolParams)


-- | Type tag used to specialize connection pool for TCP clients.
--
-- /Instance for 'Generic' introduced in version 0.1.4./
data TcpClient
  deriving (Generic, Typeable)

-- | Connection pool for TCP clients.
--
-- /Definition changed in version 0.1.3 and 0.1.4./
-- /Instances for 'Generic' and 'Show' introduced in version 0.1.4./
newtype instance ConnectionPool TcpClient =
    TcpConnectionPool (Internal.ConnectionPool HandlerParams Socket SockAddr)
  deriving (Generic, Show)

-- | /Since version 0.1.4./
instance
    HasConnectionPool HandlerParams Socket SockAddr (ConnectionPool TcpClient)
  where
    connectionPool = const TcpConnectionPool <^@~ \(TcpConnectionPool a) -> a

-- | Defined using:
--
-- @
-- 'withConnection' = 'withTcpClientConnection'
-- 'destroyAllConnections' = 'destroyAllTcpClientConnections'
-- @
--
-- /Since version 0.1.4./
instance ConnectionPoolFor TcpClient where
    type HandlerData TcpClient = AppData

    withConnection = withTcpClientConnection
    destroyAllConnections = destroyAllTcpClientConnections

-- | Create connection pool for TCP clients.
createTcpClientPool
    :: ResourcePoolParams
    -> ClientSettings
    -> IO (ConnectionPool TcpClient)
createTcpClientPool poolParams tcpParams = TcpConnectionPool
    <$> Internal.createConnectionPool handlerParams acquire release poolParams
  where
    acquire = Internal.acquireTcpClientConnection tcpParams
    release = Socket.sClose
    handlerParams = Internal.fromClientSettings tcpParams

-- | Temporarily take a TCP connection from a pool, run client with it, and
-- return it to the pool afterwards. For details how connections are allocated
-- see 'Data.Pool.withResource'.
withTcpClientConnection
    :: (MonadBaseControl io m, io ~ IO)
    => ConnectionPool TcpClient
    -> (AppData -> m r)
    -> m r
withTcpClientConnection (TcpConnectionPool pool) =
    Internal.withConnection pool . Internal.runTcpApp Nothing

-- | Destroy all TCP connections that might be still open in a connection pool.
-- This is useful when one needs to release all resources at once and not to
-- wait for idle timeout to be reached.
--
-- For more details see 'Pool.destroyAllResources'.
--
-- /Since version 0.1.1.0./
destroyAllTcpClientConnections
    :: ConnectionPool TcpClient
    -> IO ()
destroyAllTcpClientConnections (TcpConnectionPool pool) =
    Internal.destroyAllConnections pool