-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Data.CRF.Base
( Ob
, Lb
, FeatIx
, Sent
, DataSet
, HasObs (..)
-- , HasLbs (..)
-- , IsWord (..)
) where

import qualified Data.Vector as V

type Ob = Int   -- Observation
type Lb = Int   -- Label

-- | Feature index.
type FeatIx = Int

type Sent a      = V.Vector a
type DataSet a b = V.Vector (Sent a, Sent b)

-- | We use HasObs class to simplify Data.CRF.Feature module implementation.
class HasObs x where
    obs :: x -> [Ob]

-- class HasLbs x where
--     lbs :: x -> [Lb]
-- 
-- class (HasObs w, HasLbs w) => IsWord w where
-- instance (HasObs w, HasLbs w) => IsWord w where
