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
    

instance Sent Xs where
    obsOn xs k = toList $ index xs k
    sentLen = length

instance Sent XYs where
    obsOn xs k = toList $ fst $ index xs k
    sentLen = length

instance SentM XYs where
    choiceOn xs k = toList $ snd $ index xs k
