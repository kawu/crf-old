module Data.CRF.Word
( Word (..)
, ToWord
) where

data Word a = Word
    { obs       :: [a]
    , lbs       :: [a]
    , choice    :: [(a, Double)] }

type ToWord w = w -> Word Int
