module Data.CRF.XY
( XY (..)
) where

data XY a = XY
    { obs  	:: [a]
    , lbs  	:: [a]
    , choice	:: [(a, Double)] }
