module Data.CRF.XRY
( XRY (..)
) where

import qualified Data.Vector.Unboxed as U

import qualified Data.CRF.Base as B

-- | Vector of observations.
data XRY = XRY
    { unX :: U.Vector B.Ob
    , unR :: U.Vector B.Lb
    , unY :: U.Vector (B.Lb, Double) }

instance B.X XRY where
    obs = U.toList . unX

instance B.R XRY where
    interps = U.toList . unR

instance B.Y XRY where
    choice = U.toList . unY
