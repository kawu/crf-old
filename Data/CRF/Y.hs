module Data.CRF.Y
( Y (..)
, Ys
, choice
) where

import qualified Data.Vector.Unboxed as U

import Data.CRF.Base

-- TODO: Now Y can store multiple pairs of the same Lb value (because
-- we ignore base forms, which would make those elements different
-- otherwise). Perhaps we should ignore duplications?
--
-- | Simple word represented by a list of its observations.
-- It is related to Data.CRF.R type by R's invariant.
newtype Y = Y { unY :: U.Vector (Lb, Double) } deriving (Show, Read, Eq, Ord)

type Ys = Sent Y

{-# INLINE choice #-}
choice :: Y -> [(Lb, Double)]
choice = U.toList . unY
