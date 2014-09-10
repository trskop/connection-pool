{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Helper functions that aren't provided by streaming-commons.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  non-portable (CPP, FlexibleContexts, NoImplicitPrelude)
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
-- any other module of this package, and it shoud stay that way. This package
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

import Data.Maybe (Maybe)
import System.IO (IO)

import Control.Monad.Trans.Control (MonadBaseControl)
import Network.Socket (Socket, SockAddr)
import Network.Socket.ByteString (sendAll)

import Data.Streaming.Network (getSocketFamilyTCP, safeRecv)
import Data.Streaming.Network.Internal
    ( AppData(AppData)
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , AppDataUnix(AppDataUnix)
#endif
    -- !WINDOWS
    , ClientSettings(ClientSettings)
    )
import qualified Data.Streaming.Network.Internal as AppData
    (AppData(appLocalAddr', appRead', appSockAddr', appWrite'))
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
    -> Socket
    -> SockAddr
    -> m r
runTcpApp localAddr app sock addr = runTcpAppImpl localAddr sock addr app

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
    { AppData.appRead' = safeRecv sock 4096
    , AppData.appWrite' = sendAll sock
    , AppData.appSockAddr' = addr
    , AppData.appLocalAddr' = localAddr
    }

-- | Wrapper for 'getSocketFamilyTCP' that takes 'ClientSettings' instead of
-- individual parameters.
acquireTcpClientConnection :: ClientSettings -> IO (Socket, SockAddr)
acquireTcpClientConnection (ClientSettings port host addrFamily) =
    getSocketFamilyTCP host port addrFamily

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

-- | Wrapper for 'runUnixAppImpl' with a type signature that is more natural
-- for implementing a UNIX Socket specific
-- 'Data.ConnectionPool.Internal.ConnectionPool.withConnection'.
runUnixApp
    :: MonadBaseControl IO m
    => (AppDataUnix -> m r)
    -> Socket
    -> ()
    -> m r
runUnixApp app sock () = runUnixAppImpl sock app

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
