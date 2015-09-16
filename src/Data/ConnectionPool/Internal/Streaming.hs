{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Helper functions that aren't provided by streaming-commons.
-- Copyright:    (c) 2014-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  CPP, FlexibleContexts, NoImplicitPrelude
--
-- Module defines helper functions that would be ideally provided by
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons>
-- package and some wrappers with specialised type signatures.
--
-- Internal packages are here to provide access to internal definitions for
-- library writers, but they should not be used in application code.
--
-- Preferably use qualified import, e.g.:
--
-- > import qualified Data.ConnectionPool.Internal.Streaming as Internal
--
-- This module doesn't neither depend on
-- <http://hackage.haskell.org/package/resource-pool resource-pool> package nor
-- any other module of this package, and it shoud stay that way. This module
-- uses CPP to get OS specific things right. Most importantly Windows doesn't
-- support UNIX Sockets.
--
-- Please, bear above in mind when doing modifications.
module Data.ConnectionPool.Internal.Streaming
    (
    -- * TCP
      acquireTcpClientConnection
    , runTcpApp
    , runTcpAppImpl

#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.

    -- * Unix Socket
    , runUnixApp
    , runUnixAppImpl
#endif
    -- !WINDOWS
    )
  where

import Data.Maybe (Maybe(Just))
import System.IO (IO)

import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Socket (Socket, SockAddr, sClose)
import Network.Socket.ByteString (sendAll)

import Data.Streaming.Network (getSocketFamilyTCP, safeRecv)
import Data.Streaming.Network.Internal
    ( AppData(AppData)
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , AppDataUnix(AppDataUnix)
#endif
    -- !WINDOWS
    , ClientSettings(clientPort, clientHost, clientAddrFamily)
    )
import qualified Data.Streaming.Network.Internal as AppData
    ( AppData
        ( appLocalAddr'
        , appRead'
        , appSockAddr'
        , appWrite'
#if MIN_VERSION_streaming_commons(0,1,6)
        , appCloseConnection'
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
        , appRawSocket'
#endif
        ))

#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
import qualified Data.Streaming.Network.Internal as AppDataUnix
    (AppDataUnix(appReadUnix, appWriteUnix))
#endif
    -- !WINDOWS

-- | Wrapper for 'runTcpAppImpl' with a type signature that is more natural
-- for implementing a TCP specific
-- 'Data.ConnectionPool.Internal.ConnectionPool.withConnection'.
runTcpApp
    :: MonadBaseControl IO m
    => Maybe SockAddr
    -> (AppData -> m r)
    -> params
    -> Socket
    -> SockAddr
    -> m r
runTcpApp localAddr app _params sock addr =
    runTcpAppImpl localAddr sock addr app

-- | Simplified 'Data.Streaming.Network.runTCPClient' and
-- 'Data.Streaming.Network.runTCPServer' that provides only construction of
-- 'AppData' and passing it to a callback function.
runTcpAppImpl
    :: MonadBaseControl IO m
    => Maybe SockAddr
    -> Socket
    -> SockAddr
    -> (AppData -> m r)
    -> m r
runTcpAppImpl localAddr sock addr app = app AppData
    { AppData.appRead' = safeRecv sock 4096     -- :: !(IO ByteString)
    , AppData.appWrite' = sendAll sock          -- :: !(ByteString -> IO ())
    , AppData.appSockAddr' = addr               -- :: !SockAddr
    , AppData.appLocalAddr' = localAddr         -- :: !(Maybe SockAddr)
#if MIN_VERSION_streaming_commons(0,1,6)
    , AppData.appCloseConnection' = sClose sock -- :: !(IO ())
#endif
#if MIN_VERSION_streaming_commons(0,1,12)
    , AppData.appRawSocket' = Just sock         -- :: Maybe Socket
#endif
    }

-- | Wrapper for 'getSocketFamilyTCP' that takes 'ClientSettings' instead of
-- individual parameters.
acquireTcpClientConnection :: ClientSettings -> IO (Socket, SockAddr)
acquireTcpClientConnection settings = getSocketFamilyTCP host port addrFamily
  where
    port = clientPort settings
    host = clientHost settings
    addrFamily = clientAddrFamily settings

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

-- | Wrapper for 'runUnixAppImpl' with a type signature that is more natural
-- for implementing a UNIX Socket specific
-- 'Data.ConnectionPool.Internal.ConnectionPool.withConnection'.
runUnixApp
    :: MonadBaseControl IO m
    => (AppDataUnix -> m r)
    -> params
    -> Socket
    -> ()
    -> m r
runUnixApp app _params sock () = runUnixAppImpl sock app

-- | Simplified 'Data.Streaming.Network.runUnixClient' and
-- 'Data.Streaming.Network.runUnixServer' that provides only construction of
-- 'AppDataUnix' and passing it to a callback function.
runUnixAppImpl
    :: MonadBaseControl IO m
    => Socket
    -> (AppDataUnix -> m r)
    -> m r
runUnixAppImpl sock app = app AppDataUnix
    { AppDataUnix.appReadUnix = safeRecv sock 4096
    , AppDataUnix.appWriteUnix = sendAll sock
    }
#endif
    -- !WINDOWS
