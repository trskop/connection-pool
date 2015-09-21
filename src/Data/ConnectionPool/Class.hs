{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Type class for common connection pool operations.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable
-- Portability:  FlexibleContexts, NoImplicitPrelude, PolyKinds, TypeFamilies
--
-- Type class for common connection pool operations.
module Data.ConnectionPool.Class
    ( ConnectionPoolFor(..)
    )
  where

import System.IO (IO)

import Control.Monad.Trans.Control (MonadBaseControl)

import Data.ConnectionPool.Family (ConnectionPool)


-- | Type class for common connection pool operations. It intentionally
-- doesn't handle connection pool creation, which is best left to dedicated
-- smart constructors.
--
-- /Since version 0.1.4./
class ConnectionPoolFor (t :: k) where
    -- | Data passed to individual connection handler.
    type HandlerData (t :: k)

    -- | Temporarily take a connection from a pool, run handler with it, and
    -- return it to the pool afterwards.
    --
    -- /Since version 0.1.4./
    withConnection
        :: MonadBaseControl IO m
        => ConnectionPool t
        -> (HandlerData t -> m r)
        -> m r

    -- | Destroy all connections that might be still open in a connection pool.
    -- This is useful when one needs to release all resources at once and not
    -- to wait for idle timeout to be reached.
    --
    -- /Since version 0.1.4./
    destroyAllConnections :: ConnectionPool t -> IO ()
