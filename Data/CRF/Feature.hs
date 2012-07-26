module Data.CRF.Feature
( Feature (..)
, isSFeat
, isTFeat
, isOFeat
, featuresIn
) where

import Data.Binary (Binary, Get, put, get)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Vector as V

import Data.CRF.Base
import Data.CRF.Y

-- | TFeature x y:
--   * x is label corresponding to current position,
--   * y is label corresponding to previous position.
--   OFeature o x:
--   * o is observation corresponding to current position,
--   * x is label corresponding to current position.
data Feature = SFeature {-# UNPACK #-} !Lb
             | TFeature {-# UNPACK #-} !Lb {-# UNPACK #-} !Lb
             | OFeature {-# UNPACK #-} !Ob {-# UNPACK #-} !Lb
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
trFeats :: Ys -> Int -> [(Feature, Double)]
trFeats ys 0 =
    [ (SFeature x, px)
    | (x, px) <- choice (ys V.! 0) ]
trFeats ys k =
    [ (TFeature x y, px * py)
    | (x, px) <- choice (ys V.! k)
    , (y, py) <- choice (ys V.! (k-1)) ]

-- | Observation features with assigned probabilities for a given position.
obFeats :: HasObs x => Sent x -> Ys -> Int -> [(Feature, Double)]
obFeats xs ys k =
    [ (OFeature o x, px)
    | (x, px) <- choice (ys V.! k)
    , o       <- obs    (xs V.! k) ]

-- | All features with assigned probabilities for given position.
features :: HasObs x => Sent x -> Ys -> Int -> [(Feature, Double)]
features xs ys k = trFeats ys k ++ obFeats xs ys k

-- | All features with assigned probabilities in given sentence.
featuresIn :: HasObs x => Sent x -> Ys -> [(Feature, Double)]
featuresIn xs ys = concatMap (features xs ys) [0 .. V.length xs - 1]
