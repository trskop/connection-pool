{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef KIND_POLYMORPHIC_TYPEABLE_POLYKINDED_DATA_FAMILIES
-- Since ConnectionPool data family is not poly-kinded on GHC <7.10, then
-- neither can be ConnectionPoolFor type class.
{-# LANGUAGE PolyKinds #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  Type class for common connection pool operations.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable
-- Portability:  CPP, FlexibleContexts, NoImplicitPrelude, PolyKinds,
--               TypeFamilies
--
-- Type class for common connection pool operations.
module Data.ConnectionPool.Class
    ( ConnectionPoolFor(..)
    )
  where

import Data.Maybe (Maybe)
import System.IO (IO)

import Control.Monad.Trans.Control (MonadBaseControl)

import Data.ConnectionPool.Family (ConnectionPool)


-- | Type class for common connection pool operations. It intentionally
-- doesn't handle connection pool creation, which is best left to dedicated
-- smart constructors.
--
-- /Since version 0.2./
class
#ifdef KIND_POLYMORPHIC_TYPEABLE_POLYKINDED_DATA_FAMILIES
    ConnectionPoolFor (protocol :: k)
#else
    -- Since ConnectionPool data family is not poly-kinded on GHC <7.10, then
    -- neither can be ConnectionPoolFor type class.
    ConnectionPoolFor protocol
#endif
  where
    -- | Data passed to individual connection handler.
    type HandlerData protocol

    -- | Temporarily take a connection from a pool, run handler with it, and
    -- return it to the pool afterwards.
    --
    -- /Since version 0.2./
    withConnection
        :: MonadBaseControl IO m
        => ConnectionPool protocol
        -> (HandlerData protocol -> m r)
        -> m r

    -- | Similar to 'withConnection', but only performs action if a connection
    -- could be taken from the pool /without blocking./ Otherwise,
    -- 'tryWithResource' returns immediately with 'Nothing' (ie. the action
    -- function is not called). Conversely, if a connection can be acquired
    -- from the pool without blocking, the action is performed and it's result
    -- is returned, wrapped in a 'Just'.
    --
    -- /Since version 0.2./
    tryWithConnection
        :: MonadBaseControl IO m
        => ConnectionPool protocol
        -> (HandlerData protocol -> m r)
        -> m (Maybe r)

    -- | Destroy all connections that might be still open in a connection pool.
    -- This is useful when one needs to release all resources at once and not
    -- to wait for idle timeout to be reached.
    --
    -- /Since version 0.2./
    destroyAllConnections :: ConnectionPool protocol -> IO ()
