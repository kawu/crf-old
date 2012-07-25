module Data.CRF.Base
( Ob
, Lb
, FeatIx
, Sent
, DataSet
) where

import qualified Data.Vector as V

type Ob = Int   -- Observation
type Lb = Int   -- Label

-- | Feature index.
type FeatIx = Int

type Sent a     = V.Vector a
type DataSet a  = V.Vector (Sent a)
