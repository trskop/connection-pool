{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

#ifdef KIND_POLYMORPHIC_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif

#ifdef KIND_POLYMORPHIC_TYPEABLE_POLYKINDED_DATA_FAMILIES
{-# LANGUAGE PolyKinds #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  Family of connection pools specialized by transport protocol.
-- Copyright:    (c) 2014-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    stable
-- Portability:  CPP, DeriveDataTypeable, PolyKinds, StandaloneDeriving,
--               NoImplicitPrelude, TypeFamilies
--
-- Module defines data family of connection pools that is later specialised
-- for various protocols and implementations.
--
-- This module is intended mostly for library writers, for normal usage just
-- import "Data.ConnectionPool" which re-exports 'ConnectionPool' data family.
--
-- Notice that this module doesn't depend on any other internal modules nor any
-- other package then <http://hackage.haskell.org/package/base base>. Please,
-- bear this in mind when doing modifications.
module Data.ConnectionPool.Family
    (
    -- * Connection Pool Family
      ConnectionPool
    )
  where

#ifdef KIND_POLYMORPHIC_TYPEABLE
import Data.Typeable (Typeable)
#endif

-- | Family of connection pools parametrised by transport protocol.
--
-- /Definition changed version 0.1.4/ to be kind polymorphic (only on GHC >=
-- 7.10) and became part of stable API by being moved in to
-- "Data.ConnectionPool.Family" module.
data family ConnectionPool
#ifdef KIND_POLYMORPHIC_TYPEABLE_POLYKINDED_DATA_FAMILIES
    :: k -> *
#else
    :: * -> *
#endif

-- To be able to use poly-kinded ConnectionPool data family we need two things
-- (i) poly-kinded Typeable class and (ii) compiler that is capable of deriving
-- Typeable instance for poly-kinded data family. Otherwise we would break
-- backward compatibility by not providing Typeable instance. Both of these
-- work on GHC only on >=7.10.

#ifdef KIND_POLYMORPHIC_TYPEABLE
deriving instance Typeable ConnectionPool
#endif
