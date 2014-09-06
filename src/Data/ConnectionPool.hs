{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:       $HEADER$
-- Description:  Connection pools for various transport protocols.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable
-- Portability:  non-portable (CPP, FlexibleContexts, NoImplicitPrelude,
--               TupleSections)
--
-- Connection pools for TCP clients and UNIX Socket clients (not supported on
-- Windows).
--
-- This package is built on top of
-- <http://hackage.haskell.org/package/resource-pool resource-pool> and
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons>
-- packages. The later allows us to use
-- <http://hackage.haskell.org/package/conduit-extra conduit-extra> package
-- for implementing TCP and UNIX Sockets clients. Package /conduit-extra/
-- defines @appSource@ and @appSink@ based on abstractions from
-- /streaming-commons/ package and they can be therefore reused. Difference
-- between using /conduit-extra/ or /streaming-commons/ is that instead of
-- using @runTCPClient@ (or its lifted variant @runGeneralTCPClient@ from
-- /conduit-extra/) one would use 'withTcpClientConnection', and instead of
-- @runUnixClient@ it would be 'withUnixClientConnection'.
module Data.ConnectionPool
    (
    -- * Connection Pool
    --
    -- $connectionPool
      ConnectionPool

    -- * Constructing Connection Pool
    --
    -- $constructingConnectionPool
    , ResourcePoolParams
    , numberOfResourcesPerStripe
    , numberOfStripes
    , resourceIdleTimeout

    -- * TCP Client Connection Pool
    , TcpClient
    , ClientSettings
    , AppData
    , createTcpClientPool
    , withTcpClientConnection

    -- * UNIX Client Connection Pool
    , UnixClient
    , ClientSettingsUnix
    , AppDataUnix
    , createUnixClientPool
    , withUnixClientConnection
    )
  where

import Control.Applicative ((<$>))
import Data.Function ((.))
import Data.Maybe (Maybe(Nothing))
import System.IO (IO)

import qualified Network.Socket as Socket (sClose)

import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Streaming.Network
    ( AppData
    , ClientSettings
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , AppDataUnix
    , ClientSettingsUnix
    , getPath
    , getSocketUnix
#endif
    -- !WINDOWS
    )

import Data.ConnectionPool.Internal.ConnectionPoolFamily
    ( ConnectionPool
    , TcpClient
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , UnixClient
#endif
    -- !WINDOWS
    )
import qualified Data.ConnectionPool.Internal.ConnectionPool as Internal
    ( createConnectionPool
    , withConnection
    )
import qualified Data.ConnectionPool.Internal.ConnectionPoolFamily as Internal
    ( ConnectionPool(..)
    )
import Data.ConnectionPool.Internal.ResourcePoolParams
    ( ResourcePoolParams
    , numberOfResourcesPerStripe
    , numberOfStripes
    , resourceIdleTimeout
    )
import qualified Data.ConnectionPool.Internal.Streaming as Internal
    ( acquireTcpClientConnection
    , runTcpApp
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , runUnixApp
#endif
    -- !WINDOWS
    )


createTcpClientPool
    :: ResourcePoolParams
    -> ClientSettings
    -> IO (ConnectionPool TcpClient)
createTcpClientPool poolParams tcpParams = Internal.TcpConnectionPool
    <$> Internal.createConnectionPool acquire release poolParams
  where
    acquire = Internal.acquireTcpClientConnection tcpParams
    release = Socket.sClose

withTcpClientConnection
    :: MonadBaseControl IO m
    => ConnectionPool TcpClient
    -> (AppData -> m r)
    -> m r
withTcpClientConnection (Internal.TcpConnectionPool pool) =
    Internal.withConnection pool . Internal.runTcpApp Nothing

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

createUnixClientPool
    :: ResourcePoolParams
    -> ClientSettingsUnix
    -> IO (ConnectionPool UnixClient)
createUnixClientPool poolParams unixParams = Internal.UnixConnectionPool
    <$> Internal.createConnectionPool acquire release poolParams
  where
    acquire = (, ()) <$> getSocketUnix (getPath unixParams)
    release = Socket.sClose

withUnixClientConnection
    :: MonadBaseControl IO m
    => ConnectionPool UnixClient
    -> (AppDataUnix -> m r)
    -> m r
withUnixClientConnection (Internal.UnixConnectionPool pool) =
    Internal.withConnection pool . Internal.runUnixApp
#endif
    -- !WINDOWS

-- $connectionPool
--
-- For each supported protocol we have a 'ConnectionPool' data family instance
-- that is tagged with supported protocol. Currently it can be either
-- 'TcpClient' or 'UnixClient'. This way we are able to use same core
-- implementation for both and only need to deviate from common code where
-- necessary.
--
-- Under the hood we use 'Network.Socket.Socket' to represent connections and
-- that limits possible implementations of 'ConnectionPool' instances to
-- protocols supported by <http://hackage.haskell.org/package/network network>
-- package.
--
-- Those interested in details should look in to
-- "Data.ConnectionPool.Internal.ConnectionPool" and
-- "Data.ConnectionPool.Internal.ConnectionPoolFamily" modules.

-- $constructingConnectionPool
--
-- For each protocol we provide separate function that creates 'ConnectionPool'
-- instance. For TCP clients it's 'createTcpClientPool' and for UNIX Socket
-- clients it's 'createUnixClientPool' (not available on Windows).
--
-- To simplify things we provide 'ResourcePoolParams' data type that is
-- accepted by concrete constructors of 'ConnectionPool' instances and it wraps
-- all common connection pool parameters.
--
-- In example, to specify connection pool with 2 stripes with 8 connections in
-- each stripe we can use:
--
-- @
-- def & numberOfStripes .~ 2
--     & numberOfResourcesPerStripe .~ 8
-- @
--
-- Functions @&@ and @.~@ are defined by
-- <http://hackage.haskell.org/package/lens lens> package, and 'def' is a
-- method of 'Default' type class defined in
-- <http://hackage.haskell.org/package/data-default-class data-default-class>
-- package.
