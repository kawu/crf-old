module Data.CRF.Internal.XY
( XY (..)
) where

import qualified Data.Vector.Unboxed as U

import qualified Data.CRF.Base as B

-- | Vector of observations.
data XY = XY
    { unX :: U.Vector B.Ob
    , unY :: U.Vector (B.Lb, Double) }

instance B.X XY where
    obs = U.toList . unX
    lbs = const []

instance B.Y XY where
    choice = U.toList . unY
