{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
#ifdef KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Family of connection pools specialized by transport protocol.
-- Copyright:    (c) 2014-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  CPP, DeriveDataTypeable, PolyKinds, StandaloneDeriving,
--               NoImplicitPrelude, TypeFamilies
--
-- Module defines type family of connection pools that is later specialised
-- using type tags (phantom types) to specialize implementation of underlying
-- 'Internal.ConnectionPool' for various protocols.
--
-- Internal packages are here to provide access to internal definitions for
-- library writers, but they should not be used in application code.
--
-- Preferably use qualified import, e.g.:
--
-- > import qualified Data.ConnectionPool.Internal.ConnectionPoolFamily
-- >   as Internal
--
-- This module doesn't depend on
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons> and
-- other non-HaskellPlatform packages directly and it is only allowed to import
-- "Data.ConnectionPool.Internal.ConnectionPool" and
-- "Data.ConnectionPool.Internal.HandlerParams" internal module and nothing
-- else from this package. This package uses CPP to get OS specific things
-- right. Most importantly Windows doesn't support UNIX Sockets.
--
-- Please, bear above in mind when doing modifications.
module Data.ConnectionPool.Internal.ConnectionPoolFamily
    (
    -- * Connection Pool Family
      ConnectionPool(..)

    -- * Tags For Specialised Connection Pools
    , TcpClient
#ifndef WINDOWS
    -- Windows doesn't support UNIX Sockets.
    , UnixClient
#endif
    -- !WINDOWS
    )
  where

import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.Show (Show)

import Network.Socket (SockAddr, Socket)

import Data.ConnectionPool.Family (ConnectionPool)
import qualified Data.ConnectionPool.Internal.ConnectionPool as Internal
    (ConnectionPool)
import qualified Data.ConnectionPool.Internal.HandlerParams as Internal
    (HandlerParams)


-- | Type tag used to specialize connection pool for TCP clients.
data TcpClient
  deriving (Generic, Typeable)

-- | Connection pool for TCP clients.
--
-- /Definition changed in version 0.1.3 and 0.1.4./
newtype instance ConnectionPool TcpClient =
    TcpConnectionPool
        (Internal.ConnectionPool Internal.HandlerParams Socket SockAddr)
  deriving (Generic, Show)

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

-- | Type tag used to specialize connection pool for UNIX Socket clients.
data UnixClient
  deriving (Generic, Typeable)

-- | Connection pool for UNIX Socket clients.
--
-- /Definition changed in version 0.1.3 and 0.1.4./
newtype instance ConnectionPool UnixClient =
    UnixConnectionPool
        (Internal.ConnectionPool Internal.HandlerParams Socket ())
  deriving (Generic, Show)
#endif
    -- !WINDOWS
