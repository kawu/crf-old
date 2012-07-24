module Data.CRF.FeatSel
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
-- , presentFeats
-- , presentOFeats
-- , presentTFeats
-- , presentSFeats
) where

import qualified Data.Set as S
import qualified Data.Vector as V

import           Data.CRF.Base
import           Data.CRF.Feature

hiddenOFeats :: XY w => DataSet w -> [Feature]
hiddenOFeats ds =
    [OFeature o x | o <- obSet, x <- lbSet]
  where
    obSet = nub $ concatMap sentObs $ V.toList ds
    lbSet = nub $ concatMap sentLbs $ V.toList ds

hiddenTFeats :: XY w => DataSet w -> [Feature]
hiddenTFeats ds =
    [TFeature x y | x <- lbSet, y <- lbSet]
  where
    lbSet = nub $ concatMap sentLbs $ V.toList ds

hiddenSFeats :: XY w => DataSet w -> [Feature]
hiddenSFeats ds =
    [SFeature x | x <- lbSet]
  where
    lbSet = nub $ concatMap sentLbs $ V.toList ds

hiddenFeats ds
    =  hiddenOFeats ds
    ++ hiddenTFeats ds
    ++ hiddenSFeats ds

presentOFeats :: XY w => DataSet w -> [Feature]
presentOFeats ds =
    concatMap sentOFeats $ V.toList ds
  where
    sentOFeats s = concatMap (oFeatsOn s) [0 .. sentLen s - 1]
    oFeatsOn s k = [OFeature o x | o <- obsOn s k, x <- lbsOnP s k] 

presentTFeats :: XY w => DataSet w -> [Feature]
presentTFeats ds =
    concatMap sentTFeats $ V.toList ds
  where
    sentTFeats s = concatMap (tFeatsOn s) [1 .. sentLen s - 1]
    tFeatsOn s k = [TFeature x y | x <- lbsOnP s k, y <- lbsOnP s (k-1)] 

presentSFeats :: XY w => DataSet w -> [Feature]
presentSFeats ds =
    concatMap sentSFeats $ V.toList ds
  where
    sentSFeats s = [SFeature x | x <- lbsOnP s 0] 

presentFeats ds
    =  presentOFeats ds
    ++ presentTFeats ds
    ++ presentSFeats ds

sentLbs :: XY w => Sent w -> [Lb]
sentLbs s = concatMap (lbsOnA s) [0 .. sentLen s - 1]

sentObs :: XY w => Sent w -> [Ob]
sentObs s = concatMap (obsOn s) [0 .. sentLen s - 1]

lbsOnP :: XY w => Sent w -> Int -> [Lb]
lbsOnP s = map fst . choiceOn s

lbsOnA :: XY w => Sent w -> Int -> [Lb]
lbsOnA s k = lbsOn s k ++ lbsOnP s k 

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
