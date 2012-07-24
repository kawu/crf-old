module Data.CRF.XR
( XR (..)
) where

import qualified Data.Vector.Unboxed as U

import qualified Data.CRF.Base as B

-- | Vector of observations.
data XR = XR
    { unX :: U.Vector B.Ob
    , unR :: U.Vector B.Lb }

instance B.X XR where
    obs = U.toList . unX

instance B.R XR where
    interps = U.toList . unR
