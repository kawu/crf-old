module Data.CRF.X
( X (..)
) where

data X a = X
    { obs   :: [a]
    , lbs   :: [a] }
