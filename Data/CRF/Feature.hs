module Data.CRF.Feature
( Feature (..)
, isSFeat
, isTFeat
, isOFeat
, featuresIn
-- , featuresIn'
) where

import qualified Data.ListLike as L
import           Data.Binary (Binary, Get, put, get)
import           Control.Applicative ((<*>), (<$>))

import Data.CRF.Base

-- | TFeature x y:
--   * x is label corresponding to current position,
--   * y is label corresponding to previous position.
--   OFeature o x:
--   * o is observation corresponding to current position,
--   * x is label corresponding to current position.
-- TODO: Annotate as unboxed?
data Feature = SFeature !Lb
             | TFeature !Lb !Lb
             | OFeature !Ob !Lb
	     deriving (Show, Read, Eq, Ord)

instance Binary Feature where
    put (SFeature x)   = put (0 :: Int) >> put x
    put (TFeature x y) = put (1 :: Int) >> put (x, y)
    put (OFeature o x) = put (2 :: Int) >> put (o, x)
    get = do
        k <- get :: Get Int
        case k of
            0 -> SFeature <$> get
            1 -> TFeature <$> get <*> get
            2 -> OFeature <$> get <*> get

isSFeat :: Feature -> Bool
isSFeat (SFeature _) = True
isSFeat _            = False

isOFeat :: Feature -> Bool
isOFeat (OFeature _ _) = True
isOFeat _              = False

isTFeat :: Feature -> Bool
isTFeat (TFeature _ _) = True
isTFeat _              = False

-- | Features present in data together with corresponding probabilities.
--   TODO: consider doing computation in log scale (would have to change
--   Model.Internal.updateWithNumbers too).

-- | Transition features with assigned probabilities for given position.
trFeats :: SentM s => s -> Int -> [(Feature, Double)]
trFeats sent 0 =
    [ (SFeature x, px)
    | (x, px) <- choiceOn sent 0 ]
trFeats sent k =
    [ (TFeature x y, px * py)
    | (x, px) <- choiceOn sent k
    , (y, py) <- choiceOn sent (k - 1) ]

-- | Observation features with assigned probabilities for given position.
obFeats :: SentM s => s -> Int -> [(Feature, Double)]
obFeats sent k =
    [ (OFeature o x, px)
    | (x, px) <- choiceOn sent k
    , o       <- obsOn sent k ]

-- | All features with assigned probabilities for given position.
features :: SentM s => s -> Int -> [(Feature, Double)]
features sent k = trFeats sent k ++ obFeats sent k

-- | All features with assigned probabilities in given sentence.
featuresIn :: SentM s => s -> [(Feature, Double)]
-- featuresIn sent = concat $ map (features sent) [0 .. sentLen sent]
featuresIn sent = concatMap (features sent) [0 .. sentLen sent - 1]


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
