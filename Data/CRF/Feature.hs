module Data.CRF.Feature
( Feature (..)
, ToWord
, isSFeat
, isTFeat
, isOFeat
, featuresIn
) where

import Data.Binary (Binary, Get, put, get)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Vector as V

import Data.CRF.Base
import Data.CRF.Word

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
trFeats :: ToWord w -> Sent w -> Int -> [(Feature, Double)]
trFeats f sent 0 =
    [ (SFeature x, px)
    | (x, px) <- (choice.f) (sent V.! 0) ]
trFeats f sent k =
    [ (TFeature x y, px * py)
    | (x, px) <- (choice.f) (sent V.! k)
    , (y, py) <- (choice.f) (sent V.! (k-1)) ]

-- | Observation features with assigned probabilities for a given position.
obFeats :: ToWord w -> Sent w -> Int -> [(Feature, Double)]
obFeats f sent k =
    [ (OFeature o x, px)
    | (x, px) <- (choice.f) (sent V.! k)
    , o       <- (obs.f)    (sent V.! k) ]

-- | All features with assigned probabilities for given position.
features :: ToWord w -> Sent w -> Int -> [(Feature, Double)]
features f sent k = trFeats f sent k ++ obFeats f sent k

-- | All features with assigned probabilities in given sentence.
featuresIn :: ToWord w -> Sent w -> [(Feature, Double)]
featuresIn f sent = concatMap (features f sent) [0 .. V.length sent - 1]

-- trFeats' :: Sent (R, Y) -> Int -> [Feature]
-- trFeats' sent 0 =
--     [ SFeature x
--     | x <- (lbs.fst) (sent V.! 0) ]
-- trFeats' sent k =
--     [ TFeature x y
--     | x <- (lbs.fst) (sent V.! k)
--     , y <- (lbs.fst) (sent V.! (k-1)) ]

-- obFeats' :: XY w => Conns -> Sent w -> Int -> [(Feature, FeatIx)]
-- obFeats' conns sent k =
--     [ (OFeature o x, fx)
--     | o       <- obsOn sent k
--     , (x, fx) <- intersect (lbsOn sent k) (obIxs conns o) ]
-- 
-- -- | All features for given position.
-- features' :: XY w => Conns -> Sent w -> Int -> [(Feature, FeatIx)]
-- features' conns sent k
--     =  trFeats' conns sent k
--     ++ obFeats' conns sent k
-- 
-- -- | All features in given sentence.
-- featuresIn' :: SentR s => s -> [Feature]
-- featuresIn' sent = concat $ map (features' sent) [0 .. sentLen sent]
