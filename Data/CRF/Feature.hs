module Data.CRF.Feature
( Feature (..)
, isTFeat
, isOFeat
, featuresIn
-- , featuresIn'
) where

import qualified Data.ListLike as L

import Data.CRF.Base

-- | TFeature x y:
--   * x is label corresponding to current position,
--   * y is label corresponding to previous position.
--   OFeature o x:
--   * o is observation corresponding to current position,
--   * x is label corresponding to current position.
-- TODO: Annotate as unboxed?
data Feature = TFeature !Lb !Lb
             | OFeature !Ob !Lb
	     deriving (Show, Read, Eq, Ord)

isOFeat :: Feature -> Bool
isOFeat (OFeature _ _) = True
isOFeat _              = False

isTFeat :: Feature -> Bool
isTFeat = not . isOFeat


-- | Features present in data together with corresponding probabilities.
--   TODO: consider doing computation in log scale (would have to change
--   Model.Internal.updateWithNumbers too).


-- | Transition features with assigned probabilities for given position.
transitionFeatures :: SentM s => s -> Int -> [(Feature, Double)]
transitionFeatures sent k =
    [ (TFeature x y, px * py)
    | (x, px) <- choiceOn sent k
    , (y, py) <- choiceOn sent (k - 1) ]

-- | Observation features with assigned probabilities for given position.
observationFeatures :: SentM s => s -> Int -> [(Feature, Double)]
observationFeatures sent k =
    [ (OFeature o x, px)
    | (x, px) <- choiceOn sent k
    , o       <- obsOn sent k ]

-- | All features with assigned probabilities for given position.
features :: SentM s => s -> Int -> [(Feature, Double)]
features sent k = transitionFeatures sent k
               ++ observationFeatures sent k

-- | All features with assigned probabilities in given sentence.
featuresIn :: SentM s => s -> [(Feature, Double)]
featuresIn sent = concat $ map (features sent) [0 .. sentLen sent]


-- -- | Transition features for given position.
-- transitionFeatures' :: SentR s => s -> Int -> [Feature]
-- transitionFeatures' sent k =
--     [ TFeature x y
--     | x <- L.toList $ interpsOn sent k
--     , y <- L.toList $ interpsOn sent (k - 1) ]
-- 
-- -- | Observation features for given position.
-- observationFeatures' :: SentR s => s -> Int -> [Feature]
-- observationFeatures' sent k =
--     [ OFeature o x
--     | x <- L.toList $ interpsOn sent k
--     , o <- L.toList $ observationsOn sent k ]
-- 
-- -- | All features for given position.
-- features' :: SentR s => s -> Int -> [Feature]
-- features' sent k = transitionFeatures' sent k
--                 ++ observationFeatures' sent k
-- 
-- -- | All features in given sentence.
-- featuresIn' :: SentR s => s -> [Feature]
-- featuresIn' sent = concat $ map (features' sent) [0 .. sentLen sent]
