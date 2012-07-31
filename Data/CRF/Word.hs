{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Word
( Word (..)
, normalize
, rmDups
) where

import qualified Data.Set as S
import qualified Data.Map as M

data Word a b = Word
    { obs       :: [a]
    , lbs       :: [b]
    , choice    :: [(b, Double)] }
    deriving (Show, Read, Eq, Ord)

-- | Word probability normalization.
normalize :: Word a b -> Word a b
normalize word =
    word { choice = doIt (choice word) }
  where
    doIt xs =
        let z = sum (map snd xs)
        in  [(t, w / z) | (t, w) <- xs]

instance Functor (Word a) where
    fmap f Word{..} = Word
        { obs = obs
        , lbs = map f lbs
        , choice = [(f x, pr) | (x, pr) <- choice] }

-- | Remove duplicate labels.
rmDups :: Ord b => Word a b -> Word a b
rmDups Word{..} = Word
    { obs = obs
    , lbs = nubS lbs
    , choice = nubM choice }
  where
    nubS = S.toList . S.fromList
    nubM = M.toList . M.fromListWith (+)
