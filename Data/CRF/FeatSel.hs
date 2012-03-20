module Data.CRF.FeatSel
( hidden
) where

import qualified Data.ListLike as L
import qualified Data.Set as S

import           Data.CRF.Base
import           Data.CRF.Feature
import qualified Data.CRF.Const as Const

hidden :: (L.ListLike ds s, SentM s) => ds -> [Feature]
hidden ds =
    sFeats ++ tFeats ++ oFeats
  where
    sFeats = [SFeature x | x <- labelSet]
    tFeats = [TFeature x y | x <- labelSet, y <- labelSet]
    oFeats = [OFeature o x | o <- obSet   , x <- labelSet]

    obSet     = nub $ concatMap sentObs    $ L.toList ds
    labelSet  = nub $ concatMap sentLabels $ L.toList ds

    sentLabels sent = concatMap (map fst . choiceOn sent)
                                [0 .. sentLen sent - 1]
    sentObs    sent = concatMap (obsOn sent)
                                [0 .. sentLen sent - 1]

    nub = S.toList . S.fromList

-- present :: ListLike ds XYs => ds -> [Feature]
