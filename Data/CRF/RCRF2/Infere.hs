{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Data.CRF.RCRF2.Infere
( tag
-- , prob
-- , cll
-- , tagProbs
-- , tagProbs'
, accuracy
, expectedFeaturesIn
, zx
, zx'
) where

import           Control.Applicative ((<$>), (<*>))
import           Data.List (maximumBy, sortBy)
import           Data.Maybe (catMaybes)
import           Data.Function (on)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.IntMap as IM

import           Control.Parallel.Strategies (rseq, parMap)
import           Control.Parallel (par, pseq)
import           GHC.Conc (numCapabilities)

import qualified Data.CRF.Control.DynamicProgramming as DP
import           Data.CRF.Util (partition)
import           Data.CRF.LogMath (logSum, mInf, logSub, logAdd)

import           Data.CRF.Base hiding (DataSet)
import           Data.CRF.R
import           Data.CRF.Y
import           Data.CRF.Feature
import           Data.CRF.RCRF2.Model

type ProbArray = Int -> Lb -> Double
type AccF = [Double] -> Double

-- BEG Basic definitions.

{-# INLINE lbsNum #-}
lbsNum :: Sent R -> Int -> Int
lbsNum sent k = (U.length.unR) (sent V.! k)

{-# INLINE labelIxs #-}
labelIxs :: Sent R -> Int -> [(Int, Lb)]
labelIxs sent k = zip [0..] $ (U.toList.unR) (sent V.! k)

-- {-# INLINE labelIxs' #-}
-- labelIxs' :: Sent R -> Int -> [Int]
-- labelIxs' sent k = [0 .. lbsNum sent k - 1]

{-# INLINE labelOn #-}
labelOn :: R -> Int -> Lb
labelOn r k = unR r U.! k

-- | Assumption: labels (in [(Int, Lb)] list) are given in
-- an ascending order.  NOTE: we do not use this assumption
-- right now, but when we change the implementation it can
-- be helpful.
intersect
    :: IxMap            -- ^ Map from a label to feature index
    -> [(Int, Lb)]      -- ^ Label index and the label itself
    -> [(Int, FeatIx)]  -- ^ Label index and feature index
intersect ixMap xs = catMaybes
    -- | Naive implementation for now.
    [ (k,) <$> IM.lookup x ixMap
    | (k, x) <- xs ]

-- END Basic definitions.

-- | General model methods.

-- | TODO: It may me faster with Data.Map (but not in general case).
-- Or we can choose data structure depending on the restricted set (size)?
-- Perhaps adding better memory managment would be enough.
computePsi :: Model -> Sent R -> Int -> Lb -> Double
computePsi crf sent i = (A.!) arr
  where
    val     = values crf    -- ^ TODO: Check if it helps
    bounds  = (0, lbsNum sent i - 1)
    arr = A.accumArray (+) 0.0 bounds
        [ (k, val U.! ix)
        | ob <- obs (sent V.! i)
        , (k, ix) <- intersect (obIxs crf V.! ob) (labelIxs sent i) ]

-- | Forward table computation.
forward :: Model -> Sent R -> ProbArray
forward crf sent = alpha where
    alpha = DP.flexible2 (0, V.length sent) bounds
        (\t i -> withMem (computePsi crf sent i) t i)
    bounds i
        | i == V.length sent = (0, 0)
        | otherwise = (0, lbsNum sent i - 1)
    withMem psi alpha i
        | i == V.length sent = const u
        | i == 0 = \j ->
            let x = labelOn (sent V.! i) j
            in  psi j + sgValue crf x
        | otherwise = \j ->
            let x = labelOn (sent V.! i) j
            in  psi j + ((u `logSub` v x) `logAdd` w x)
      where
        u = logSum
            [ alpha (i-1) k
            | (k, _) <- labelIxs sent (i-1) ]
        v x = logSum
            [ alpha (i-1) k
            | (k, _) <- intersect (prevIxs crf V.! x) (labelIxs sent (i-1)) ]
        w x = logSum
            [ alpha (i-1) k + values crf U.! ix
            | (k, ix) <- intersect (prevIxs crf V.! x) (labelIxs sent (i-1)) ]

backward :: Model -> Sent R -> ProbArray
backward crf sent = beta where
    beta = DP.flexible2 (0, V.length sent) bounds
        (\t i -> withMem (computePsi crf sent i) t i)
    bounds i
        | i == 0    = (0, 0)
        | otherwise = (0, lbsNum sent (i-1) - 1)
    withMem psi beta i
        | i == V.length sent = const 0
        | i == 0 = const $ logSum
            [ beta (i+1) k + psi k
            + sgValue crf (labelOn (sent V.! i) k)
            | (k, _) <- labelIxs sent i ]
        | otherwise = \j ->
            let y = labelOn (sent V.! (i-1)) j
            in  (u `logSub` v y) `logAdd` w y
      where
        u = logSum
            [ beta (i+1) k + psi k
            | (k, _ ) <- labelIxs sent i ]
        v y = logSum
            [ beta (i+1) k + psi k
            | (k, _ ) <- intersect (nextIxs crf V.! y) (labelIxs sent i) ]
        w y = logSum
            [ beta (i+1) k + psi k + values crf U.! ix
            | (k, ix) <- intersect (nextIxs crf V.! y) (labelIxs sent i) ]


zxBeta :: ProbArray -> Double
zxBeta beta = beta 0 0

zxAlpha :: Sent R -> ProbArray -> Double
zxAlpha sent alpha = alpha (V.length sent) 0

zx :: Model -> Sent R -> Double
zx crf = zxBeta . backward crf

zx' :: Model -> Sent R -> Double
zx' crf sent = zxAlpha sent $ forward crf sent

argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

-- tagProbs :: Sent s => Model -> s -> [[Double]]
-- tagProbs crf sent =
--     let alpha = forward maximum crf sent
--         beta = backward maximum crf sent
--         normalize vs =
--             let d = - logSum vs
--             in map (+d) vs
--         m1 k x = alpha k x + beta (k + 1) x
--     in  [ map exp $ normalize [m1 i k | k <- interpIxs sent i]
--         | i <- [0 .. V.length sent - 1] ]

-- | Log probabilities with respect to marginal distributions.
tagProbs :: Model -> Sent R -> [[(Lb, Double)]]
tagProbs crf sent =
    let alpha = forward crf sent
        beta = backward crf sent
    in  [ [ (x, prob1 crf alpha beta sent i k)
          | (k, x) <- labelIxs sent i ]
        | i <- [0 .. V.length sent - 1] ]

-- | Get (at most) k best tags for each word and return them in
-- descending order.  FIXME: Tagging with respect to marginal
-- distributions might not be the best idea.  Think of some
-- more elegant method.
tagK :: Int -> Model -> Sent R -> [[(Lb, Double)]]
tagK k crf sent = map
    ( take k
    . reverse
    . sortBy (compare `on` snd)
    ) (tagProbs crf sent)

tag :: Model -> Sent R -> [Lb]
tag crf = map (fst.head) . (tagK 1 crf)

goodAndBad :: Model -> Sent R -> Sent Y -> (Int, Int)
goodAndBad crf sent labels =
    foldl gather (0, 0) $ zip labels' labels''
  where
    labels' = [ fst $ maximumBy (compare `on` snd)
                    $ choice (labels V.! i)
              | i <- [0 .. V.length labels - 1] ]
    labels'' = tag crf sent
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)

type DataSet = [(Sent R, Sent Y)]

goodAndBad' :: Model -> DataSet -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) [goodAndBad crf x y | (x, y) <- dataset]

-- | Parallel accuracy computation.
accuracy :: Model -> DataSet -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

prob1 :: Model -> ProbArray -> ProbArray -> Sent R -> Int -> Lb -> Double
prob1 crf alpha beta sent k x =
    alpha k x + beta (k + 1) x - zxBeta beta

prob2 crf alpha beta sent k psiMem x y ix
    = alpha (k - 1) y + beta (k + 1) x + psiMem x
    + values crf U.! ix - zxBeta beta

expectedFeaturesOn :: Model -> ProbArray -> ProbArray -> Sent R
                   -> Int -> [(FeatIx, Double)]
expectedFeaturesOn crf alpha beta sent i =
    tFeats ++ oFeats
  where
    psiMem = computePsi crf sent i
    pr1 = prob1 crf alpha beta sent i
    pr2 = prob2 crf alpha beta sent i psiMem

    oFeats = [ (ix, pr1 k) 
             | o <- obs (sent V.! i)
             , (k, ix) <- intersect (obIxs crf V.! o) (labelIxs sent i) ]

    tFeats
        | i == 0 = catMaybes
            [ (, pr1 k) <$> featToIx crf (SFeature x)
            | (k, x) <- labelIxs sent i ]
        | otherwise =
            [ (ix, pr2 k l ix)
            | (k,  x) <- labelIxs sent i
            , (l, ix) <- intersect (prevIxs crf V.! x) (labelIxs sent (i-1)) ]

expectedFeaturesIn :: Model -> Sent R -> [(FeatIx, Double)]
expectedFeaturesIn crf sent = zx `par` zx' `pseq` zx `pseq`
    concat [expectedOn i | i <- [0 .. V.length sent - 1] ]
  where
    expectedOn = expectedFeaturesOn crf alpha beta sent
    alpha = forward crf sent
    beta = backward crf sent
    zx  = zxAlpha sent alpha
    zx' = zxBeta beta
