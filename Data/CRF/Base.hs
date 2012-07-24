{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.CRF.Base
( Ob
, Lb
, FeatIx

, X (..)
, Y (..)
, XY

, Sent
, sentLen
, obsOn
, lbsOn
, choiceOn

, DataSet
) where

import qualified Data.Vector as V

type Ob = Int   -- Observation
type Lb = Int   -- Label

-- | Feature index.
type FeatIx = Int

-- | Assumption: list of labels (lbs) is given in an ascending order.
class X w where
    obs     :: w -> [Ob]
    lbs     :: w -> [Lb]

class Y w where
    choice  :: w -> [(Lb, Double)]

class (X w, Y w) => XY w where
instance (X w, Y w) => XY w where

type Sent a = V.Vector a

{-# INLINE sentLen #-}
sentLen :: Sent w -> Int
sentLen = V.length

{-# INLINE obsOn #-}
obsOn :: X w => Sent w -> Int -> [Ob]
obsOn sent k = obs (sent V.! k)

{-# INLINE lbsOn #-}
lbsOn :: X w => Sent w -> Int -> [Lb]
lbsOn sent k = lbs (sent V.! k)

{-# INLINE choiceOn #-}
choiceOn :: Y w => Sent w -> Int -> [(Lb, Double)]
choiceOn sent k = choice (sent V.! k)

type DataSet a = V.Vector (Sent a)
