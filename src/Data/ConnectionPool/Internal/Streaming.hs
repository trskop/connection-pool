{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Helper functions that aren't provided by streaming-commons.
-- Copyright:    (c) 2014-2016, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  GHC specific language extensions.
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
-- any other module of this package, with notable exception of
-- "Data.ConnectionPool.Internal.HandlerParams", and it shoud stay that way.
-- This module uses CPP to get OS specific things right. Most importantly
-- Windows doesn't support UNIX Sockets.
--
-- Please, bear above in mind when doing modifications.
module Data.ConnectionPool.Internal.Streaming
    (
    -- * TCP
      acquireTcpClientConnection
    , runTcpApp
    , runTcpAppImpl
    , fromClientSettings

#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.

    -- * Unix Socket
    , runUnixApp
    , runUnixAppImpl
    , fromClientSettingsUnix
#endif
    -- !WINDOWS
    )
  where

import Data.Int (Int)
import Data.Maybe (Maybe(Just))
import System.IO (IO)

import Network.Socket (Socket, SockAddr, sClose)
import Network.Socket.ByteString (sendAll)

import Data.Default.Class (Default(def))
import Data.Streaming.Network
    ( getSocketFamilyTCP
    , safeRecv
#if MIN_VERSION_streaming_commons(0,1,13)
    -- Until streaming-commons 0.1.13, read buffer size was fixed.
    , getReadBufferSize
#endif
    )
import Data.Streaming.Network.Internal
    ( AppData(AppData)
    , ClientSettings(clientPort, clientHost, clientAddrFamily)
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , AppDataUnix(AppDataUnix)
    , ClientSettingsUnix
#if MIN_VERSION_streaming_commons(0,1,13) \
  && !MIN_VERSION_streaming_commons(0,1,14)
        -- Until streaming-commons 0.1.13, read buffer size was fixed.
        -- ClientSettingsUnix instance for HasReadBufferSize was introduced in
        -- streaming-commons version 0.1.14 and it provides much more stable
        -- API then accessor.
        ( clientReadBufferSizeUnix
        )
#endif
    -- streaming-commons >=0.1.13 && <= 0.1.14
#endif
    -- !WINDOWS
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

import Data.ConnectionPool.Internal.HandlerParams
    ( HandlerParams(_readBufferSize)
    )

#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
import qualified Data.Streaming.Network.Internal as AppDataUnix
    (AppDataUnix(appReadUnix, appWriteUnix))
#endif
    -- !WINDOWS


-- | Wrapper for 'runTcpAppImpl' with a type signature that is more natural
-- for implementing a TCP specific
-- 'Data.ConnectionPool.Internal.ConnectionPool.withConnection'.
--
-- /Definition changed in version 0.1.3 and 0.2.1./
runTcpApp
    :: Maybe SockAddr
    -> (AppData -> m r)
    -> HandlerParams
    -- ^ Parameters passed down to connection handler @('AppData' -> m r)@ as
    -- part of definition of 'AppData'.
    -- /Since version 0.1.3./
    -> Socket
    -> SockAddr
    -> m r
runTcpApp localAddr app params sock addr =
    runTcpAppImpl localAddr sock addr bufSize app
  where
    bufSize = _readBufferSize params
{-# INLINE runTcpApp #-}

-- | Simplified 'Data.Streaming.Network.runTCPClient' and
-- 'Data.Streaming.Network.runTCPServer' that provides only construction of
-- 'AppData' and passing it to a callback function.
--
-- /Definition changed in version 0.1.3 and 0.2.1./
runTcpAppImpl
    :: Maybe SockAddr
    -> Socket
    -> SockAddr
    -> Int
    -- ^ Buffer size used while reading from socket. /Since version 0.1.3./
    -> (AppData -> m r)
    -> m r
runTcpAppImpl localAddr sock addr bufSize app = app AppData
    { AppData.appRead' = safeRecv sock bufSize  -- :: !(IO ByteString)
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
{-# INLINE runTcpAppImpl #-}

-- | Wrapper for 'getSocketFamilyTCP' that takes 'ClientSettings' instead of
-- individual parameters.
acquireTcpClientConnection :: ClientSettings -> IO (Socket, SockAddr)
acquireTcpClientConnection settings = getSocketFamilyTCP host port addrFamily
  where
    port = clientPort settings
    host = clientHost settings
    addrFamily = clientAddrFamily settings
{-# INLINEABLE acquireTcpClientConnection #-}

-- | Construct 'HandlerParams' that are passed to individual TCP connection
-- handlers.
--
-- /Since version 0.1.3./
fromClientSettings :: ClientSettings -> HandlerParams
fromClientSettings _tcpParams = def
#if MIN_VERSION_streaming_commons(0,1,13)
    -- Until streaming-commons 0.1.13, read buffer size was fixed.
    { _readBufferSize = getReadBufferSize _tcpParams
    }
#endif
{-# INLINE fromClientSettings #-}

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

-- | Wrapper for 'runUnixAppImpl' with a type signature that is more natural
-- for implementing a UNIX Socket specific
-- 'Data.ConnectionPool.Internal.ConnectionPool.withConnection'.
--
-- /Definition changed in version 0.1.3 and 0.2.1./
runUnixApp
    :: (AppDataUnix -> m r)
    -> HandlerParams
    -- ^ Parameters passed down to connection handler @('AppDataUnix' -> m r)@ as
    -- part of definition of 'AppDataUnix'.
    -- /Since version 0.1.3./
    -> Socket
    -> ()
    -> m r
runUnixApp app params sock () = runUnixAppImpl sock bufSize app
  where
    bufSize = _readBufferSize params
{-# INLINE runUnixApp #-}

-- | Simplified 'Data.Streaming.Network.runUnixClient' and
-- 'Data.Streaming.Network.runUnixServer' that provides only construction of
-- 'AppDataUnix' and passing it to a callback function.
--
-- /Definition changed in version 0.1.3 and 0.2.1./
runUnixAppImpl
    :: Socket
    -> Int
    -- ^ Buffer size used while reading from socket. /Since version 0.1.3./
    -> (AppDataUnix -> m r)
    -> m r
runUnixAppImpl sock bufSize app = app AppDataUnix
    { AppDataUnix.appReadUnix = safeRecv sock bufSize
    , AppDataUnix.appWriteUnix = sendAll sock
    }
{-# INLINE runUnixAppImpl #-}

-- | Construct 'HandlerParams' that are passed to individual UNIX socket
-- connection handlers.
--
-- /Since version 0.1.3./
fromClientSettingsUnix :: ClientSettingsUnix -> HandlerParams
fromClientSettingsUnix _unixParams = def
#if MIN_VERSION_streaming_commons(0,1,13)
    -- Until streaming-commons 0.1.13, read buffer size was fixed.
    { _readBufferSize =
#if MIN_VERSION_streaming_commons(0,1,14)
        -- ClientSettingsUnix instance for HasReadBufferSize was introduced in
        -- streaming-commons version 0.1.14 and it provides much more stable
        -- API then accessor.
        getReadBufferSize _unixParams
#else
        -- In streaming-commons 0.1.13 we have to use clientReadBufferSizeUnix
        -- accessor of ClientSettingsUnix.
        clientReadBufferSizeUnix _unixParams
#endif
    }
#endif
    -- streaming-commons >= 0.1.13
#endif
    -- !WINDOWS
{-# INLINE fromClientSettingsUnix #-}
