module Data.CRF.Y
( Y (..)
, choice
) where

import qualified Data.Vector.Unboxed as U

import Data.CRF.Base

-- | Simple word represented by a list of its observations.
newtype Y = Y { unY :: U.Vector (Lb, Double) }

{-# INLINE choice #-}
choice :: Y -> [(Lb, Double)]
choice = U.toList . unY
