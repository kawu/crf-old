{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances #-}

module Data.CRF.Types
( X (..)
, R (..)
, Y (..)
, Sent (..)
, SentM (..)
, SentR (..)
, SentRM (..)
, Label
, Obser
) where

import qualified Data.ListLike as L
import           Data.ListLike.Vector
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

type Obser = Int    -- Observation
type Label = Int    -- Label

-- | Set of observations.
type X = U.Vector Obser     -- type X = [Int]
-- | Set of labels.
type R = U.Vector Label
-- | Set of labels with corresponding probabilities.
type Y = U.Vector (Label, Double)

-- Concerns functions below: For positions outside sentence boundaries,
-- special 'dummy' label and empty observations set is used.

-- | Sequence of words: sentence
class Sent s where
    -- | Observations on given position.
    observationsOn :: s -> Int -> X
    -- | Sentence length.
    sentLen :: s -> Int

-- | Sentence with known possible interpretations (restrictions).
class Sent s => SentR s where
    -- | Interpretations on given position.
    interpsOn :: s -> Int -> R
    -- | Indices of individual, interpretations for given position.
    interpIxs :: s -> Int -> [Int]
    -- | Indices of individual, neighboring interpretations
    -- for given position.
    interpIxs2 :: s -> Int -> [(Int, Int)]
    -- | Number of interpretations on a given position.
    interpsNum :: s -> Int -> Int
    -- | Interpretation by position and index.
    interp :: s -> Int -> Int -> Int

    -- | Default implementations.
    interpIxs sent k = [0 .. L.length (interpsOn sent k) - 1]
    interpIxs2 sent k = 
        [(i, j) | i <- interpIxs sent k
                , j <- interpIxs sent $ k - 1]
    interpsNum sent = L.length . interpsOn sent
    interp sent k = L.index $ interpsOn sent k

-- | Sentence with chosen labels (each label with corresponding probability).
class Sent s => SentM s where
    choiceOn :: s -> Int -> Y
    
-- | Sentence with restrictions and chosen labels.
class (SentR s, SentM s) => SentRM s where
instance (SentR s, SentM s) => SentRM s where
