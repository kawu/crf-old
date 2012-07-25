module Data.CRF.FeatSel
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
, presentFeats
, presentOFeats
, presentTFeats
, presentSFeats
) where

import qualified Data.Set as S
import qualified Data.Vector as V

import Data.CRF.Base
import Data.CRF.Word
import Data.CRF.Feature

hiddenOFeats :: ToWord w -> DataSet w -> [Feature]
hiddenOFeats f ds =
    [OFeature o x | o <- obSet, x <- lbSet]
  where
    obSet = nub $ concatMap (sentObs f) $ V.toList ds
    lbSet = nub $ concatMap (sentLbs f) $ V.toList ds

hiddenTFeats :: ToWord w -> DataSet w -> [Feature]
hiddenTFeats f ds =
    [TFeature x y | x <- lbSet, y <- lbSet]
  where
    lbSet = nub $ concatMap (sentLbs f) $ V.toList ds

hiddenSFeats :: ToWord w -> DataSet w -> [Feature]
hiddenSFeats f ds =
    [SFeature x | x <- lbSet]
  where
    lbSet = nub $ concatMap (sentLbs f) $ V.toList ds

hiddenFeats f ds
    =  hiddenOFeats f ds
    ++ hiddenTFeats f ds
    ++ hiddenSFeats f ds

presentOFeats :: ToWord w -> DataSet w -> [Feature]
presentOFeats f ds =
    concatMap sentOFeats $ V.toList ds
  where
    sentOFeats s = concatMap (oFeatsOn s) [0 .. V.length s - 1]
    oFeatsOn s k =
        [ OFeature o x
        | o <- (obs.f)  (s V.! k)
        , x <- (lbsP.f) (s V.! k) ]

presentTFeats :: ToWord w -> DataSet w -> [Feature]
presentTFeats f ds =
    concatMap sentTFeats $ V.toList ds
  where
    sentTFeats s = concatMap (tFeatsOn s) [1 .. V.length s - 1]
    tFeatsOn s k =
        [ TFeature x y
        | x <- (lbsP.f) (s V.! k)
        , y <- (lbsP.f) (s V.! (k-1)) ]

presentSFeats :: ToWord w -> DataSet w -> [Feature]
presentSFeats f ds =
    concatMap sentSFeats $ V.toList ds
  where
    sentSFeats s = [SFeature x | x <- (lbsP.f) (s V.! 0)] 

presentFeats f ds
    =  presentOFeats f ds
    ++ presentTFeats f ds
    ++ presentSFeats f ds

sentObs :: ToWord w -> Sent w -> [Ob]
sentObs f s = concatMap (obs.f.(s V.!)) [0 .. V.length s - 1]

sentLbs :: ToWord w -> Sent w -> [Lb]
sentLbs f s = concatMap (lbsA.f.(s V.!)) [0 .. V.length s - 1]

lbsA :: Word Int -> [Lb]
lbsA w = lbs w ++ lbsP w

lbsP :: Word Int -> [Lb]
lbsP = map fst . choice

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
