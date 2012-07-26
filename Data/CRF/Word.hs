module Data.CRF.Word
( Word (..)
) where

data Word a b = Word
    { obs       :: [a]
    , lbs       :: [b]
    , choice    :: [(b, Double)] }
