module Data.CRF.Internal.X
( X (..)
) where

import qualified Data.Vector.Unboxed as U

import qualified Data.CRF.Base as B

-- | Vector of observations.
newtype X = X { unX :: U.Vector B.Ob }

instance B.X X where
    obs = U.toList . unX
    lbs = const []
