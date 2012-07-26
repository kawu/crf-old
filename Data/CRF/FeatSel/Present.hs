module Data.CRF.FeatSel.Present
( presentFeats
, presentOFeats
, presentTFeats
, presentSFeats
) where

import qualified Data.Vector as V

import Data.CRF.Base
import Data.CRF.Y
import Data.CRF.Feature

presentOFeats :: HasObs x => DataSet x Y -> [Feature]
presentOFeats ds =
    concatMap sentOFeats $ V.toList ds
  where
    sentOFeats (xs, ys) = concatMap oFeatsOn (zip (V.toList xs) (V.toList ys))
    oFeatsOn (w, choice) =
        [ OFeature o y
        | o <- obs w
        , y <- lbs choice ]

presentTFeats :: DataSet x Y -> [Feature]
presentTFeats ds =
    concatMap (sentTFeats.snd) (V.toList ds)
  where
    sentTFeats ys = concatMap (tFeatsOn ys) [1 .. V.length ys - 1]
    tFeatsOn ys k =
        [ TFeature x y
        | x <- lbs (ys V.! k)
        , y <- lbs (ys V.! (k-1)) ]

presentSFeats :: DataSet x Y -> [Feature]
presentSFeats ds =
    concatMap (sentSFeats.snd) $ V.toList ds
  where
    sentSFeats s = [SFeature x | x <- lbs (s V.! 0)] 

presentFeats :: HasObs x => DataSet x Y -> [Feature]
presentFeats ds
    =  presentOFeats ds
    ++ presentTFeats ds
    ++ presentSFeats ds

lbs :: Y -> [Lb]
lbs = map fst . choice
