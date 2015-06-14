{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
#ifdef KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE StandaloneDeriving #-}
#endif
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module:       $HEADER$
-- Description:  Family of connection pools specialized by transport protocol.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  non-portable (CPP, DeriveDataTypeable, StandaloneDeriving,
--               NoImplicitPrelude, TypeFamilies)
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
-- "Data.ConnectionPool.Internal.ConnectionPool" internal module and nothing
-- else from this module. This package uses CPP to get OS specific things
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

import Network.Socket (SockAddr)

import qualified Data.ConnectionPool.Internal.ConnectionPool as Internal
    (ConnectionPool)


-- | Family of connection pools parametrised by transport protocol.
data family ConnectionPool :: * -> *

#ifdef KIND_POLYMORPHIC_TYPEABLE
deriving instance Typeable ConnectionPool
#endif

-- | Type tag used to specialize connection pool for TCP clients.
data TcpClient
  deriving Typeable

-- | Connection pool for TCP clients.
newtype instance ConnectionPool TcpClient =
    TcpConnectionPool (Internal.ConnectionPool SockAddr)

#ifndef WINDOWS
-- Windows doesn't support UNIX Sockets.

-- | Type tag used to specialize connection pool for UNIX Socket clients.
data UnixClient
  deriving Typeable

-- | Connection pool for UNIX Socket clients.
newtype instance ConnectionPool UnixClient =
    UnixConnectionPool (Internal.ConnectionPool ())
#endif
    -- !WINDOWS
