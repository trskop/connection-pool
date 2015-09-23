{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Main
-- Description:  UNIX Sockets client example.
-- Copyright:    (c) 2014-2015, Peter Trško
-- License:      BSD3
module Main (main)
  where

import Control.Concurrent
    ( forkIO
    , newEmptyMVar
    , putMVar
    , readMVar
    , threadDelay
    )
import Control.Monad (void, mapM_)
import System.Environment (getArgs)

import Control.Lens ((.~), (&))
import Data.ConnectionPool
    ( createTcpClientPool
    , numberOfResourcesPerStripe
    , numberOfStripes
    , withTcpClientConnection
    )
import Data.Default.Class (Default(def))
import Data.Streaming.Network (appWrite, clientSettingsTCP)


main :: IO ()
main = do
    [port, numStripes, numPerStripe] <- getArgs
    pool <- createTcpClientPool
        (poolParams numStripes numPerStripe)
        (clientSettingsTCP (read port) "127.0.0.1")
    thread1 <- newEmptyMVar
    thread2 <- newEmptyMVar
    void . forkIO . withTcpClientConnection pool $ \appData -> do
        threadDelay 1000
        appWrite appData "1: I'm alive!\n"
        putMVar thread1 ()
    void . forkIO . withTcpClientConnection pool $ \appData -> do
        appWrite appData "2: I'm alive!\n"
        putMVar thread2 ()
    mapM_ readMVar [thread1, thread2]
  where
    poolParams m n =
        def & numberOfStripes .~ read m
            & numberOfResourcesPerStripe .~ read n
