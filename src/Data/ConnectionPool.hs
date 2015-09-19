{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:       $HEADER$
-- Description:  Connection pools for various transport protocols.
-- Copyright:    (c) 2014-2015, Peter Trško
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
    -- * TCP Client Example
    --
    -- $tcpClientExample

#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.

    -- * Unix Client Example
    --
    -- $unixClientExample
#endif
    -- !WINDOWS

    -- * Connection Pool
    --
    -- $connectionPool
      ConnectionPool

    -- * Constructing Connection Pool
    --
    -- $constructingConnectionPool
    , ResourcePoolParams

    -- ** Lenses
    --
    -- $resourcePoolParamsLenses
    , numberOfResourcesPerStripe
    , numberOfStripes
    , resourceIdleTimeout

    -- ** Validation
    --
    -- $resourcePoolParamsValidation
    , validateResourcePoolParams

    -- * TCP Client Connection Pool
    , TcpClient
    , ClientSettings
    , AppData
    , createTcpClientPool
    , withTcpClientConnection
    , destroyAllTcpClientConnections

#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.

    -- * UNIX Client Connection Pool
    , UnixClient
    , ClientSettingsUnix
    , AppDataUnix
    , createUnixClientPool
    , withUnixClientConnection
    , destroyAllUnixClientConnections
#endif
    -- !WINDOWS
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
    , destroyAllConnections
    )
import qualified Data.ConnectionPool.Internal.ConnectionPoolFamily as Internal
    ( ConnectionPool(..)
    )
import Data.ConnectionPool.Internal.ResourcePoolParams
    ( ResourcePoolParams
    , numberOfResourcesPerStripe
    , numberOfStripes
    , resourceIdleTimeout
    , validateResourcePoolParams
    )
import qualified Data.ConnectionPool.Internal.Streaming as Internal
    ( acquireTcpClientConnection
    , fromClientSettings
    , runTcpApp
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , fromClientSettingsUnix
    , runUnixApp
#endif
    -- !WINDOWS
    )


-- | Create connection pool for TCP clients.
createTcpClientPool
    :: ResourcePoolParams
    -> ClientSettings
    -> IO (ConnectionPool TcpClient)
createTcpClientPool poolParams tcpParams = Internal.TcpConnectionPool
    <$> Internal.createConnectionPool handlerParams acquire release poolParams
  where
    acquire = Internal.acquireTcpClientConnection tcpParams
    release = Socket.sClose
    handlerParams = Internal.fromClientSettings tcpParams

-- | Temporarily take a TCP connection from a pool, run client with it, and
-- return it to the pool afterwards. For details how connections are allocated
-- see 'Data.Pool.withResource'.
withTcpClientConnection
    :: MonadBaseControl IO m
    => ConnectionPool TcpClient
    -> (AppData -> m r)
    -> m r
withTcpClientConnection (Internal.TcpConnectionPool pool) =
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
destroyAllTcpClientConnections (Internal.TcpConnectionPool pool) =
    Internal.destroyAllConnections pool

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

-- | Create connection pool for UNIX Sockets clients.
createUnixClientPool
    :: ResourcePoolParams
    -> ClientSettingsUnix
    -> IO (ConnectionPool UnixClient)
createUnixClientPool poolParams unixParams = Internal.UnixConnectionPool
    <$> Internal.createConnectionPool handlerParams acquire release poolParams
  where
    acquire = (, ()) <$> getSocketUnix (getPath unixParams)
    release = Socket.sClose
    handlerParams = Internal.fromClientSettingsUnix unixParams

-- | Temporarily take a UNIX Sockets connection from a pool, run client with
-- it, and return it to the pool afterwards. For details how connections are
-- allocated see 'Data.Pool.withResource'.
withUnixClientConnection
    :: MonadBaseControl IO m
    => ConnectionPool UnixClient
    -> (AppDataUnix -> m r)
    -> m r
withUnixClientConnection (Internal.UnixConnectionPool pool) =
    Internal.withConnection pool . Internal.runUnixApp

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
destroyAllUnixClientConnections (Internal.UnixConnectionPool pool) =
    Internal.destroyAllConnections pool
#endif
    -- !WINDOWS

-- $tcpClientExample
--
-- Here is a simple example that demonstrates how TCP client can be created and
-- how connection pool behaves.
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- module Main (main)
--   where
--
-- import Control.Monad (void)
-- import Control.Concurrent (forkIO, threadDelay)
-- import System.Environment (getArgs)
--
-- import Control.Lens ((.~), (&))
-- import Data.ConnectionPool
--     ( 'createTcpClientPool'
--     , 'numberOfResourcesPerStripe'
--     , 'numberOfStripes'
--     , 'withTcpClientConnection'
--     )
-- import Data.Default.Class (Default(def))
-- import Data.Streaming.Network
--     ( 'Data.Streaming.Network.appWrite'
--     , 'Data.Streaming.Network.clientSettingsTCP'
--     )
--
--
-- main :: IO ()
-- main = do
--     [port, numStripes, numPerStripe] <- getArgs
--     pool <- 'createTcpClientPool'
--         (poolParams numStripes numPerStripe)
--         ('Data.Streaming.Network.clientSettingsTCP' (read port) \"127.0.0.1\")
--     void . forkIO . 'withTcpClientConnection' pool $ \\appData -> do
--        threadDelay 100
--        'Data.Streaming.Network.appWrite' appData \"1: I'm alive!\\n\"
--     void . forkIO . 'withTcpClientConnection' pool $ \\appData ->
--        'Data.Streaming.Network.appWrite' appData \"2: I'm alive!\\n\"
--   where
--     poolParams m n =
--         'Data.Default.Class.def' & 'numberOfStripes' .~ read m
--             & 'numberOfResourcesPerStripe' .~ read n
-- @
--
-- To test it we can use @socat@ or some @netcat@ like application. Our test
-- will require two terminals, in one we will execute @socat@ as a server
-- listenting on UNIX socket and in the other one we execute above example.
--
-- Simple TCP server listening on port @8001@ that prints what it receives to
-- stdout:
--
-- > $ socat TCP4-LISTEN:8001,bind=127.0.0.1,fork -
--
-- The @fork@ parameter in the above example is important, otherwise @socat@
-- would terminate when client closes its connection.
--
-- If we run above example as:
--
-- > $ runghc tcp-example.hs 8001 1 1
--
-- We can see that @socat@ received following text:
--
-- > 1: I'm alive!
-- > 2: I'm alive!
--
-- But if we increment number of stripes or number of connections (resources)
-- per stripe, then we will get:
--
-- > 2: I'm alive!
-- > 1: I'm alive!
--
-- The reason for this is that we use @threadDelay 100@ in the first executed
-- thread. So when we have only one stripe and one connection per stripe, then
-- we have only one connection in the pool. Therefore when the first thread
-- executes and acquires a connection, then all the other threads (the other
-- one in above example) will block. If we have more then one connection
-- available in our pool, then the first thread acquires connection, blocks on
-- @threadDelay@ call, but the other thread also acquires connection and prints
-- its output while the first thread is still blocked on @threadDelay@. This
-- example demonstrates how connection pool behaves if it reached its capacity
-- and when it has enough free resources.

-- $unixClientExample
--
-- Here is a simple example that demonstrates how UNIX Sockets client can be
-- created and how connection pool behaves.
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- module Main (main)
--   where
--
-- import Control.Concurrent (forkIO, threadDelay)
-- import Control.Monad (void)
-- import System.Environment (getArgs)
--
-- import Control.Lens ((.~), (&))
-- import Data.ConnectionPool
--     ( 'createUnixClientPool'
--     , 'numberOfResourcesPerStripe'
--     , 'numberOfStripes'
--     , 'withUnixClientConnection'
--     )
-- import Data.Default.Class (Default(def))
-- import Data.Streaming.Network
--     ( 'Data.Streaming.Network.appWrite'
--     , 'Data.Streaming.Network.clientSettingsUnix'
--     )
--
--
-- main :: IO ()
-- main = do
--     [socket, numStripes, numPerStripe] <- getArgs
--     pool <- 'createUnixClientPool'
--         (poolParams numStripes numPerStripe)
--         ('Data.Streaming.Network.clientSettingsUnix' socket)
--     void . forkIO . 'withUnixClientConnection' pool $ \\appData -> do
--        threadDelay 100
--        'Data.Streaming.Network.appWrite' appData \"1: I'm alive!\\n\"
--     void . forkIO . 'withUnixClientConnection' pool $ \\appData ->
--        'Data.Streaming.Network.appWrite' appData \"2: I'm alive!\\n\"
--   where
--     poolParams m n =
--         'Data.Default.Class.def' & 'numberOfStripes' .~ read m
--             & 'numberOfResourcesPerStripe' .~ read n
-- @
--
-- Above example is very similar to our TCP Client Example and most notably the
-- implementation of two client threads is the same. Testing it is very similar
-- to testing TCP Client Example, but we would use different command for
-- @socat@ and for executing the example.
--
-- Simple UNIX socket server that prints what it receives to stdout:
--
-- > $ socat UNIX-LISTEN:test.sock,fork -
--
-- Parameter @fork@ has the same importance as when we used it in the command
-- for running TCP server.
--
-- We can execute UNIX Sockets Example using:
--
-- > $ runghc unix-sockets-example.hs test.sock 1 1
--
-- Result of the test will be the same in case of using one stripe and one
-- connection per stripe, and when we increase total number connections, to
-- what we had with the TCP Client Example.

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
-- In each case two kinds of values need to be provided as parameters to such
-- functions:
--
-- 1. Parameters of underlying resource pool like how to organize stripes and
--    parameters for algorithm that handles resource releasing, etc.
--
-- 2. Transport protocol parameters like IP address, port, UNIX Socket file,
--    and similar.
--
-- To simplify things we provide 'ResourcePoolParams' data type that is
-- accepted by concrete constructors of 'ConnectionPool' instances and it wraps
-- all common connection pool parameters. And for protocol specific settings
-- this package reuses data types from /streaming-commons/ library.
--
-- As a result, of the above, type signature of function that creates
-- connection pool for some protocol named @MyProtocol@ could look like:
--
-- @
-- createMyProtocolPool
--     :: 'ResourcePoolParams'
--     -> MyProtocolParams
--     -> 'IO' ('ConnectionPool' MyProtocol)
-- @
--
-- To further simplify things this package defines default value for
-- 'ResourcePoolParams' using 'Data.Default.Class.Default' type class that has
-- only one method named 'Data.Default.Class.def'. Instance of this class is
-- declared using minimal possible values of each parameter required by
-- underlying resource pool. In example, to specify connection pool with 2
-- stripes with 8 connections in each stripe, but keeping connection idle
-- timeout on its default value, we can simply use:
--
-- @
-- 'Data.Default.Class.def' & 'numberOfStripes' .~ 2
--     & 'numberOfResourcesPerStripe' .~ 8
-- @
--
-- Where functions @&@ and @.~@ are defined by
-- <http://hackage.haskell.org/package/lens lens> package.

-- $resourcePoolParamsLenses
--
-- For details on how to use leses as these see
-- <http://hackage.haskell.org/package/lens lens> package where you might find
-- a good starting point documentation for you.

-- $resourcePoolParamsValidation
--
-- Sometimes one needs to validate parameters as early as possible, e.g. while
-- parsing command line options.
--
-- Usage example:
--
-- @
-- 'validateResourcePoolParams' $ someParams
--     & 'resourceIdleTimeout' .~ 1
--     & 'numberOfResourcesPerStripe' .~ 16
-- @
--
-- Most usually one would use 'Data.Default.def' instead of @someParams@.
-- Functions @&@ and @.~@ are defined in
-- <http://hackage.haskell.org/package/lens lens> package.
