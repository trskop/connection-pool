{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:       Main
-- Description:  UNIX Sockets client example.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
module Main (main)
  where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import System.Environment (getArgs)

import Control.Lens ((.~), (&))
import Data.ConnectionPool
    ( createUnixClientPool
    , numberOfResourcesPerStripe
    , numberOfStripes
    , withUnixClientConnection
    )
import Data.Default.Class (Default(def))
import Data.Streaming.Network (appWrite, clientSettingsUnix)


main :: IO ()
main = do
    [socket, numStripes, numPerStripe] <- getArgs
    pool <- createUnixClientPool
        (poolParams numStripes numPerStripe)
        (clientSettingsUnix socket)
    void . forkIO . withUnixClientConnection pool $ \appData -> do
       threadDelay 100
       appWrite appData "1: I'm alive!\n"
    void . forkIO . withUnixClientConnection pool $ \appData ->
       appWrite appData "2: I'm alive!\n"
  where
    poolParams m n =
        def & numberOfStripes .~ read m
            & numberOfResourcesPerStripe .~ read n
