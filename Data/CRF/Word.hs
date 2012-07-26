{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Word
( Word (..)
) where

data Word a b = Word
    { obs       :: [a]
    , lbs       :: [b]
    , choice    :: [(b, Double)] }
    deriving (Show, Read, Eq, Ord)

instance Functor (Word a) where
    fmap f Word{..} = Word
        { obs = obs
        , lbs = map f lbs
        , choice = [(f x, pr) | (x, pr) <- choice] }
