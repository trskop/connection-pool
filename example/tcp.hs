{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Main
-- Description:  UNIX Sockets client example.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
module Main (main)
  where

import Control.Monad (void)
import Control.Concurrent (forkIO, threadDelay)
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
    void . forkIO . withTcpClientConnection pool $ \appData -> do
       threadDelay 100
       appWrite appData "1: I'm alive!\n"
    void . forkIO . withTcpClientConnection pool $ \appData ->
       appWrite appData "2: I'm alive!\n"
  where
    poolParams m n =
        def & numberOfStripes .~ read m
            & numberOfResourcesPerStripe .~ read n
