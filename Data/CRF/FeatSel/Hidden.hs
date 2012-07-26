module Data.CRF.FeatSel.Hidden
( hiddenFeats
, hiddenOFeats
, hiddenTFeats
, hiddenSFeats
) where

import qualified Data.Set as S
import qualified Data.Vector as V

import Data.CRF.Base
import Data.CRF.R
import Data.CRF.Y
import Data.CRF.Feature

hiddenOFeats :: DataSet R Y -> [Feature]
hiddenOFeats ds =
    [OFeature o x | o <- obSet, x <- lbSet]
  where
    obSet = nub $ concatMap (sentObs . fst)   $ V.toList ds
    lbSet = nub $ concatMap (uncurry sentLbs) $ V.toList ds

hiddenTFeats :: DataSet R Y -> [Feature]
hiddenTFeats ds =
    [TFeature x y | x <- lbSet, y <- lbSet]
  where
    lbSet = nub $ concatMap (uncurry sentLbs) $ V.toList ds

hiddenSFeats :: DataSet R Y -> [Feature]
hiddenSFeats ds =
    [SFeature x | x <- lbSet]
  where
    lbSet = nub $ concatMap (uncurry sentLbs) $ V.toList ds

hiddenFeats :: DataSet R Y -> [Feature]
hiddenFeats ds
    =  hiddenOFeats ds
    ++ hiddenTFeats ds
    ++ hiddenSFeats ds

collectObs :: DataSet R Y -> [Ob]
collectObs = nub . concatMap (sentObs . fst) . V.toList

collectLbs :: DataSet R Y -> [Lb]
collectLbs = nub . concatMap (uncurry sentLbs) . V.toList

sentObs :: Sent R -> [Ob]
sentObs rs = concatMap obs (V.toList rs)

sentLbs :: Sent R -> Ys -> [Lb]
sentLbs rs ys = concat
    [lbsAll r y | (r, y) <- zip (V.toList rs) (V.toList ys)]

lbsAll :: R -> Y -> [Lb]
lbsAll r y = lbs r ++ map fst (choice y)

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList
