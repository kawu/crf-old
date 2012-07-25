module Data.CRF.RCRF.Feature
( labelIxs
, labelIxs2
, sgFeatFor
, trFeatFor
, obFeatFor
) where

import Control.Applicative ((<*>), (<$>))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Data.CRF.Base
import Data.CRF.Feature
import Data.CRF.R

lbsNum :: Int -> Sent R -> Int -> Int
lbsNum n sent k
    | m > 0     = m
    | otherwise = n
  where
    m = (U.length.unR) (sent V.! k)

-- | Label indices for a given position. If restriction set is empty,
-- set of all possible labels is taken (assuming that the first argument
-- is equall to a number of labels).
labelIxs :: Int -> Sent R -> Int -> [Int]
labelIxs n sent k = [0 .. lbsNum n sent k - 1]

labelOn :: Int -> R -> Int -> Lb
labelOn n r k
    | m > 0     = v U.! k
    | otherwise = k
  where
    v = unR r
    m = U.length v

labelIxs2 :: Int -> Sent R -> Int -> [(Int, Int)]
labelIxs2 n s 0 = error "labelIxs2: k == 0"
labelIxs2 n s k = 
    [ (i, j)
    | i <- labelIxs n s k
    , j <- labelIxs n s (k-1) ]

sgFeatFor :: Int -> Sent R -> Int -> Feature
sgFeatFor n sent = SFeature . labelOn n (sent V.! 0)

trFeatFor :: Int -> Sent R -> Int -> (Int, Int) -> Feature
trFeatFor _ _    0 _      = error "trFeatFor: k == 0"
trFeatFor n sent k (a, b) =
    TFeature x y
  where
    x = labelOn n (sent V.! k) a
    y = labelOn n (sent V.! (k-1)) b

obFeatFor :: Int -> Sent R -> Int -> Int -> [Feature]
obFeatFor n sent k a =
    [ OFeature o x
    | o <- obs (sent V.! k) ]
  where
    x = labelOn n (sent V.! k) a
