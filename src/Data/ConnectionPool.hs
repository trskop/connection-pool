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
module Data.ConnectionPool
    (
    -- * Connection Pool
      ConnectionPool

    -- * Constructing Connection Pool
    , ResourcePoolParams
    , numberOfResourcesPerStripe
    , numberOfStripes
    , resourceIdleTimeout

    -- * TCP Client Connection Pool
    , TcpClient
    , createTcpClientPool
    , withTcpClientConnection

    -- * UNIX Client Connection Pool
    , UnixClient
    , createUnixClientPool
    , withUnixClientConnection
    )
  where

import Control.Applicative (Const(Const, getConst), (<$>))
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
    , HasPath(pathLens)
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
    acquire = (, ()) <$> getSocketUnix (unixParams ^. pathLens)
    release = Socket.sClose
    s ^. l = getConst (l Const s)

withUnixClientConnection
    :: MonadBaseControl IO m
    => ConnectionPool UnixClient
    -> (AppDataUnix -> m r)
    -> m r
withUnixClientConnection (Internal.UnixConnectionPool pool) =
    Internal.withConnection pool . Internal.runUnixApp
#endif
    -- !WINDOWS
