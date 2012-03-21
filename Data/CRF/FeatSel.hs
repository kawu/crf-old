module Data.CRF.FeatSel
( hidden
, hiddenTFeats
, hiddenOFeats
, hiddenSFeats
, present
, presentTFeats
, presentOFeats
) where

import qualified Data.ListLike as L
import qualified Data.Set as S

import           Data.CRF.Base
import           Data.CRF.Feature

hiddenOFeats :: (L.ListLike ds s, SentM s) => ds -> [Feature]
hiddenOFeats ds =
    [OFeature o x | o <- obSet, x <- lbSet]
  where
    obSet = nub $ concatMap sentObs $ L.toList ds
    lbSet = nub $ concatMap sentLbs $ L.toList ds

hiddenTFeats :: (L.ListLike ds s, SentM s) => ds -> [Feature]
hiddenTFeats ds =
    [TFeature x y | x <- lbSet, y <- lbSet]
  where
    lbSet = nub $ concatMap sentLbs $ L.toList ds

hiddenSFeats :: (L.ListLike ds s, SentM s) => ds -> [Feature]
hiddenSFeats ds =
    [SFeature x | x <- lbSet]
  where
    lbSet = nub $ concatMap sentLbs $ L.toList ds

hidden ds = hiddenOFeats ds
         ++ hiddenTFeats ds
         ++ hiddenSFeats ds

presentTFeats :: (L.ListLike ds s, SentM s) => ds -> [Feature]
presentTFeats ds =
    concatMap sentTFeats $ L.toList ds
  where
    sentTFeats s = concatMap (tFeatsOn s) [1 .. sentLen s - 1]
    tFeatsOn s k = [TFeature x y | x <- lbsOn s k, y <- lbsOn s (k-1)] 

presentOFeats :: (L.ListLike ds s, SentM s) => ds -> [Feature]
presentOFeats ds =
    concatMap sentOFeats $ L.toList ds
  where
    sentOFeats s = concatMap (oFeatsOn s) [0 .. sentLen s - 1]
    oFeatsOn s k = [OFeature o x | o <- obsOn s k, x <- lbsOn s k] 

-- | FIXME: Cannot use presentSFeats -- see
-- Module/Module.Internal implementation!
present ds = presentOFeats ds
          ++ presentTFeats ds
          -- ++ hiddenTFeats ds
          ++ hiddenSFeats ds

sentLbs :: SentM s => s -> [Lb]
sentLbs s = concatMap (lbsOn s) [0 .. sentLen s - 1]

sentObs :: SentM s => s -> [Ob]
sentObs s = concatMap (obsOn s) [0 .. sentLen s - 1]

lbsOn s = map fst . choiceOn s
nub = S.toList . S.fromList
