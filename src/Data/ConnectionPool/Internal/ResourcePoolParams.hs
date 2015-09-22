{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
-- Module:       $HEADER$
-- Description:  Resource pool construction parameters.
-- Copyright:    (c) 2014, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    unstable (internal module)
-- Portability:  DeriveDataTypeable, NoImplicitPrelude, RecordWildCards
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
-- crippling functionality.
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

    -- ** Validation
    , validateResourcePoolParams
    )
  where

import Control.Monad (Monad(return))
import Data.Bool (otherwise)
import Data.Data (Data)
import Data.Either (Either(Left, Right))
import Data.Function (($))
import Data.Functor (Functor)
import Data.Int (Int)
import Data.List ((++))
import Data.Ord (Ord((<)))
import Data.String (String)
import Data.Typeable (Typeable)
import Text.Show (Show(show))

import Data.Time.Clock (NominalDiffTime)

import Data.Default.Class (Default(def))
import Data.Function.Between.Strict ((~@@^>))


-- | Parameters of resource pool that describe things like its internal
-- structure. See 'Data.Pool.createPool' for details.
data ResourcePoolParams = ResourcePoolParams
    { _numberOfStripes :: !Int
    , _resourceIdleTimeout :: !NominalDiffTime
    , _numberOfResourcesPerStripe :: !Int
    }
  deriving (Data, Show, Typeable)

-- | @
-- 'numberOfStripes' = 1
-- 'resourceIdleTimeout' = 0.5
-- 'numberOfResourcesPerStripe' = 1
-- @
instance Default ResourcePoolParams where
    def = ResourcePoolParams
        { _numberOfStripes = 1
        , _resourceIdleTimeout = 0.5
        , _numberOfResourcesPerStripe = 1
        }
    {-# INLINE def #-}

-- | Lens for accessing stripe count. The number of distinct sub-pools to
-- maintain. The smallest acceptable value is 1 (default).
numberOfStripes
    :: Functor f
    => (Int -> f Int)
    -> ResourcePoolParams -> f ResourcePoolParams
numberOfStripes =
    _numberOfStripes ~@@^> \s b -> s {_numberOfStripes = b}
{-# INLINE numberOfStripes #-}

-- | Lens for accessing amount of time for which an unused resource is kept
-- open. The smallest acceptable value is 0.5 seconds (default).
resourceIdleTimeout
    :: Functor f
    => (NominalDiffTime -> f NominalDiffTime)
    -> ResourcePoolParams -> f ResourcePoolParams
resourceIdleTimeout = _resourceIdleTimeout ~@@^> \s b ->
    s {_resourceIdleTimeout = b}
{-# INLINE resourceIdleTimeout #-}

-- | Lens for accessing maximum number of resources to keep open per stripe.
-- The smallest acceptable value is 1 (default).
numberOfResourcesPerStripe
    :: Functor f
    => (Int -> f Int)
    -> ResourcePoolParams -> f ResourcePoolParams
numberOfResourcesPerStripe = _numberOfResourcesPerStripe ~@@^> \s b ->
    s {_numberOfResourcesPerStripe = b}
{-# INLINE numberOfResourcesPerStripe #-}

-- | Check if all parameters for underlying resource pool are valid:
--
-- * @'numberOfStripes' >= 1@ Number of connection sub-pools. Keeping it set
--   to @1@ is good for most applications.
--
-- * @'numberOfResourcesPerStripe' >= 1@ Maximum number of connections in each
--   stripe. Totally there can be
--   @'numberOfStripes' * 'numberOfResourcesPerStripe'@
--   open connections simultaneously.
--
-- * @'resourceIdleTimeout' >= 0.5@ Property specified for how long connection
--   will be kept alive after it is released by back to the pool before it is
--   automatically closed. Value is in seconds.
--
-- For more details see 'Data.Pool.createPool'.
--
-- /Since version 0.1.1.0./
validateResourcePoolParams
    :: ResourcePoolParams
    -- ^ Parameters to validate.
    -> Either String ResourcePoolParams
    -- ^ Either error message or the same value of 'ResourcePoolParams' passed
    -- as a first argument.
validateResourcePoolParams params'@ResourcePoolParams{..} = do
    failIf _numberOfStripes (< 1)
        "Stripe count has to be at least 1"
    failIf _resourceIdleTimeout (< 0.5)
        "Resource idle time has to be at least 0.5"
    failIf _numberOfResourcesPerStripe (< 1)
        "There has to be at least 1 resource per stripe"
    return params'
  where
    failIf n p msg
      | p n = Left $ msg ++ ", but got " ++ show n ++ "."
      | otherwise = Right ()
