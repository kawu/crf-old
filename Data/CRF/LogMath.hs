{-# LANGUAGE ForeignFunctionInterface #-}

module Data.CRF.LogMath
( logIsZero
, logAdd
, logSub
, logSum
, inf
, mInf
) where

import Data.List (foldl', sort)

foreign import ccall unsafe "math.h log1p"
    log1p :: Double -> Double

inf :: RealFloat a => a
inf = (1/0)

mInf :: RealFloat a => a
mInf = -(1/0)

{-# INLINE logIsZero #-}
logIsZero :: Double -> Bool
logIsZero x = x == mInf

{-# INLINE logAdd #-}
logAdd :: Double -> Double -> Double
logAdd x y
    | logIsZero x   = y
    | x > y         = x + log1p(exp(y - x))
    | otherwise     = y + log1p(exp(x - y))

{-# INLINE logSub #-}
logSub :: Double -> Double -> Double
logSub x y = x + log1p (negate (exp (y - x)))

logSum :: [Double] -> Double
logSum = foldl' logAdd mInf . sort
