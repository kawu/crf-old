{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , TypeSynonymInstances
           , UndecidableInstances #-}

module Data.CRF.Base
( X
, Y
, Xs
, XYs
, Sent (..)
, SentM (..)
, Lb
, Ob
) where

import           Prelude hiding (length)
import           Data.ListLike (toList, length, index)
import           Data.ListLike.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

type Ob = Int   -- Observation
type Lb = Int   -- Label

-- | Vector of observations.
type X = U.Vector Ob
-- | Vector of labels with corresponding probabilities.
type Y = U.Vector (Lb, Double)

-- | Sentence -- sequence of words.
type Xs  = V.Vector X
-- | Sentence with chosen interpretations.
type XYs = V.Vector (X, Y)


-- | Sequence of words: sentence
class Sent s where
    -- | Observations on given position.
    obsOn   :: s -> Int -> [Ob]
    -- | Sentence length.
    sentLen :: s -> Int

-- | Sentence with chosen labels (each label with corresponding probability).
class Sent s => SentM s where
    choiceOn :: s -> Int -> [(Lb, Double)]
    

-- | FIXME: There is no need to check k position.  Change also
-- Data.CRF.Model (and Internal?) module.
instance Sent Xs where
    obsOn xs k
        | k < 0 || k >= length xs   = []
        | otherwise                 = toList $ index xs k
    sentLen = length

instance Sent XYs where
    obsOn xs k
        | k < 0 || k >= length xs   = []
        | otherwise                 = toList $ fst $ index xs k
    sentLen = length

instance SentM XYs where
    choiceOn xs k
        | k < 0 || k >= length xs   = undefined
        | otherwise                 = toList $ snd $ index xs k


-- Following function only have sense, when set of all labels in data
-- set is known and CRF model is provided as argument.
-- -- | Indices of individual, interpretations for given position.
-- interpIxs :: SentR s => s -> Int -> [Int]
-- interpIxs sent k = [0 .. L.length (interpsOn sent k) - 1]
-- 
-- -- | Number of interpretations on a given position.
-- interpsNum :: SentR s => s -> Int -> Int
-- interpsNum sent = L.length . interpsOn sent
-- 
-- -- | Interpretation by position and index.
-- interp :: SentR s => s -> Int -> Int -> Int
-- interp sent k = L.index $ interpsOn sent k
