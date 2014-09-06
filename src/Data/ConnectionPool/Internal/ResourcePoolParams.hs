{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module:       $HEADER$
-- Description:  Resource pool construction parameters.
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  non-portable (NoImplicitPrelude, DeriveDataTypeable)
--
-- Internal packages are here to provide access to internal definitions for
-- library writers, but they should not be used in application code.
--
-- Preferably use qualified import, e.g.:
--
-- > import qualified Data.ConnectionPool.Internal.ResourcePoolParams
-- >   as Internal
--
-- Surprisingly this module doesn't depend on
-- <http://hackage.haskell.org/package/resource-pool resource-pool>
-- package and it would be good if it stayed that way, but not at the cost of
-- cripling functionality.
--
-- Importantly this package should not depend on
-- <http://hackage.haskell.org/package/streaming-commons streaming-commons>
-- package or other modules of this package.
--
-- Please, bear above in mind when doing modifications.
module Data.ConnectionPool.Internal.ResourcePoolParams
    (
    -- * ResourcePoolParams
      ResourcePoolParams(..)

    -- ** Lenses
    , numberOfResourcesPerStripe
    , numberOfStripes
    , resourceIdleTimeout
    )
  where

import Data.Data (Data)
import Data.Functor (Functor)
import Data.Int (Int)
import Data.Typeable (Typeable)
import Text.Show (Show)

import Data.Time.Clock (NominalDiffTime)

import Data.Default.Class (Default(def))
import Data.Function.Between ((~@@^>))


-- | Parameters of resource pool that describe things like its internal
-- structure. See 'Data.Pool.createPool' for details.
data ResourcePoolParams = ResourcePoolParams
    { _numberOfStripes :: !Int
    , _resourceIdleTimeout :: !NominalDiffTime
    , _numberOfResourcesPerStripe :: !Int
    }
  deriving (Data, Show, Typeable)

-- | @
-- numberOfStripes = 1
-- resourceIdleTimeout = 0.5
-- numberOfResourcesPerStripe = 1
-- @
instance Default ResourcePoolParams where
    def = ResourcePoolParams
        { _numberOfStripes = 1
        , _resourceIdleTimeout = 0.5
        , _numberOfResourcesPerStripe = 1
        }

-- | Lens for accessing stripe count. The number of distinct sub-pools to
-- maintain. The smallest acceptable value is 1 (default).
numberOfStripes
    :: Functor f
    => (Int -> f Int)
    -> ResourcePoolParams -> f ResourcePoolParams
numberOfStripes =
    _numberOfStripes ~@@^> \s b -> s {_numberOfStripes = b}

-- | Lens for accessing amount of time for which an unused resource is kept
-- open. The smallest acceptable value is 0.5 seconds (default).
resourceIdleTimeout
    :: Functor f
    => (NominalDiffTime -> f NominalDiffTime)
    -> ResourcePoolParams -> f ResourcePoolParams
resourceIdleTimeout = _resourceIdleTimeout ~@@^> \s b ->
    s {_resourceIdleTimeout = b}

-- | Lens for accessing maximum number of resources to keep open per stripe.
-- The smallest acceptable value is 1 (default).
numberOfResourcesPerStripe
    :: Functor f
    => (Int -> f Int)
    -> ResourcePoolParams -> f ResourcePoolParams
numberOfResourcesPerStripe = _numberOfResourcesPerStripe ~@@^> \s b ->
    s {_numberOfResourcesPerStripe = b}
