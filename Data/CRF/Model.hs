{-# LANGUAGE FlexibleContexts #-}

module Data.CRF.Model
( module Data.CRF.Model.Internal
, tag
-- -- , prob
-- -- , cll
-- , tagProbs
-- , tagProbs'
, accuracy
-- , accuracy'
, expectedFeaturesIn
, zx
, zx'
) where

-- import Data.MemoCombinators (memo2, integral)
import qualified Data.Array as Array
import Data.List (maximumBy)
import Data.Function (on)
import qualified Data.ListLike as L

import Control.Parallel (par, pseq)
import Control.Parallel.Strategies ( using, parList, parBuffer, evalList
                                   , evalTuple2, rseq, parMap )

import qualified Data.CRF.Control.DynamicProgramming as DP
import Data.CRF.Util (partition)
import Data.CRF.LogMath (logSum)
import Data.CRF.Base
import Data.CRF.Const (dummy)
import Data.CRF.Feature
import Data.CRF.Model.Internal

import Debug.Trace (trace)

type ProbArray = Int -> Lb -> Double
type AccF = [Double] -> Double

(!) :: L.ListLike full item => full -> Int -> item 
(!) = L.index

-- | Interface on top of internal implementation.

-- onWord :: Model -> X -> Label -> Double
-- onWord crf w x = 
--     sum [onOFeat o x crf | o <- L.toList w]
-- 
-- onTransition :: Model -> Label -> Label -> Double
-- onTransition crf x y = onTFeat x y crf
-- 
-- phi :: Model -> X -> Label -> Label -> Double
-- phi crf w x y = onWord crf w x + onTransition crf x y

-- | More general methods.

-- computePsiMem :: SentR s => Model -> s -> Int -> LabelIx -> Double
-- computePsiMem crf sent i = (Array.!) $ Array.array bounds
--     [ (k, psi crf w k)
--     | k <- interpIxs sent i ]
--   where
--     psi crf w = onWord crf w . interp sent i
--     bounds = (0, interpsNum sent i - 1)
--     w = observationsOn sent i

-- | TODO: Change to Vector.
computePsiMem :: Sent s => Model -> s -> Int -> Lb -> Double
computePsiMem crf sent i = (Array.!) $ Array.accumArray (+) 0.0 bounds
    [ (lb, values crf ! ix)
    -- [ (lb, 0.0)
    | ob <- sent `obsOn` i
    , (lb, ix) <- L.toList $ obIxs crf ! ob ]
  where
    -- | FIXME: Bounds depend on position!
    bounds = (0, lbNum crf - 1)

forward :: Sent s => AccF -> Model -> s -> ProbArray
forward acc crf sent =
    alpha
  where
    alpha = DP.flexible2 (0, sentLen sent) wordBounds
                (\t k -> withMem (computePsiMem crf sent k) t k)

    wordBounds k
        | k == sentLen sent = (0, 0)
        | otherwise = (0, lbNum crf - 1)

    -- | FIXME: null sentence?
    withMem psiMem alpha k x
        | k == 0 = psiMem x + values crf ! (sgIxs crf ! x)
        | k == sentLen sent = acc
            [ alpha (k - 1) y
            | y <- [0 .. lbNum crf - 1] ]
        | otherwise = acc
            [ alpha (k - 1) y + psiMem x + values crf ! ix
            | (y, ix) <- L.toList $ prevIxs crf ! x ]

backward :: Sent s => AccF -> Model -> s -> ProbArray
backward acc crf sent =
    beta
  where
    beta = DP.flexible2 (0, sentLen sent) wordBounds
               (\t k -> withMem (computePsiMem crf sent k) t k)

    wordBounds k
        | k == 0    = (0, 0)
        | otherwise = (0, lbNum crf - 1)

    withMem psiMem beta k y
        | k == sentLen sent = 0.0
        | k == 0    = acc
            [ beta (k + 1) x + psiMem x + values crf ! ix
            | (x, ix) <- zip [0..] $ L.toList $ sgIxs crf ]
        | otherwise = acc
            [ beta (k + 1) x + psiMem x + values crf ! ix
            | (x, ix) <- L.toList $ nextIxs crf ! y ]

zxBeta :: ProbArray -> Double
zxBeta beta = beta 0 0

zxAlpha :: Sent s => s -> ProbArray -> Double
zxAlpha sent alpha = alpha (sentLen sent) 0

zx :: Sent s => Model -> s -> Double
zx crf = zxBeta . backward logSum crf

zx' :: Sent s => Model -> s -> Double
zx' crf sent = zxAlpha sent $ forward logSum crf sent

--------------------------------------------------------------
argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

-- memoTag :: Sent s => Model -> s -> [Int]
-- memoTag crf sent = snd $ alpha 0 0 where
--     n = sentLen sent
--     alpha = memo2 integral integral alpha' 
--     alpha' i v
--         | i == n = (0, [])
--         | otherwise = maximum [ (phi' i u v, u) `plus` alpha (i + 1) u
--                               | u <- interpIxs sent i ]
--     plus (v, x) (v', xs) = (v + v', x : xs)
--     phi' i u v = phi crf os a b
--         where os = observationsOn sent i
--               a = interp sent i       u
--               b = interp sent (i - 1) v

-- dynamicTag :: Sent s => Model -> s -> [Lb]
-- dynamicTag crf sent = collectMaxArg (0, 0) [] mem where
--     mem = DP.flexible2 (0, sentLen sent)
--                        (\k   -> (dummy, labelNum crf - 1))
--                        (\t k -> withMem (computePsiMem crf sent k) t k)
--     withMem psiMem mem k x
--         | k == sentLen sent = (-1, 0.0)
--         | otherwise = prune $ argmax eval $ L.toList $ nextIxs crf ! x
--       where
--         eval (y, ix) = (snd $ mem (k + 1) y) + psiMem y + values crf ! ix
--         prune ((y, ix), v) = (y, v)
--     collectMaxArg (i, j) acc mem =
--         collect $ mem i j
--       where
--         collect (h, _)
--             | h == -1 = reverse acc
--             | otherwise = collectMaxArg (i + 1, h) (h:acc) mem

dynamicTag :: Sent s => Model -> s -> [Lb]
dynamicTag crf sent =
    collectMaxArg (0, 0) [] mem
  where
    mem = DP.flexible2 (0, sentLen sent) wordBounds
               (\t k -> withMem (computePsiMem crf sent k) t k)

    wordBounds k
        | k == 0    = (0, 0)
        | otherwise = (0, lbNum crf - 1)

    withMem psiMem mem k y
        | k == sentLen sent = (-1, 0.0)
        | k == 0    = prune $ argmax eval $ L.toList
                    $ zip [0..] $ L.toList $ sgIxs crf
        | otherwise = prune $ argmax eval $ L.toList
                    $ nextIxs crf ! y
      where
        eval (x, ix) = (snd $ mem (k + 1) x) + psiMem x + values crf ! ix
        prune ((x, ix), v) = (x, v)

    collectMaxArg (i, j) acc mem =
        collect $ mem i j
      where
        collect (h, _)
            | h == -1 = reverse acc
            | otherwise = collectMaxArg (i + 1, h) (h:acc) mem

tag :: Sent s => Model -> s -> [Lb]
tag = dynamicTag

-- tagProbs :: Sent s => Model -> s -> [[Double]]
-- tagProbs crf sent =
--     let alpha = forward maximum crf sent
--         beta = backward maximum crf sent
--         normalize vs =
--             let d = - logSum vs
--             in map (+d) vs
--         m1 k x = alpha k x + beta (k + 1) x
--     in  [ map exp $ normalize [m1 i k | k <- interpIxs sent i]
--         | i <- [0 .. sentLen sent - 1] ]
-- 
-- -- tag probabilities with respect to
-- -- marginal distributions
-- tagProbs' :: Sent s => Model -> s -> [[Double]]
-- tagProbs' crf sent =
--     let alpha = forward logSum crf sent
--         beta = backward logSum crf sent
--     in  [ [ exp $ prob1 crf alpha beta sent i k
--           | k <- interpIxs sent i ]
--         | i <- [0 .. sentLen sent - 1] ]

goodAndBad :: SentM s => Model -> s -> (Int, Int)
goodAndBad crf sent =
    foldl gather (0, 0) $ zip labels labels'
  where
    labels = [ fst $ maximumBy (compare `on` snd)
                   $ L.toList $ choiceOn sent i
             | i <- [0 .. sentLen sent - 1] ]
    labels' = tag crf sent
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)

goodAndBad' :: SentM s => Model -> [s] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) $ map (goodAndBad crf) dataset

accuracy :: SentM s => Model -> [s] -> Double
accuracy crf dataset = fromIntegral good / fromIntegral (good + bad)
    where (good, bad) = goodAndBad' crf dataset

-- -- parallel implementation
-- accuracy' :: SentRM s => Int -> Model -> [s] -> Double
-- accuracy' k crf dataset =
--     let parts = partition k dataset
--         xs = parMap rseq (goodAndBad' crf) parts
--         (good, bad) = foldl add (0, 0) xs
--         add (g, b) (g', b') = (g + g', b + b')
--     in  fromIntegral good / fromIntegral (good + bad)

-- --------------------------------------------------------------

-- prob :: L.Vect t Int => Model -> Sent Int t -> Double
-- prob crf sent =
--     sum [ phiOn crf sent k
--         | k <- [0 .. (length sent) - 1] ]
--     - zx' crf sent
-- 
-- -- TODO: Wziac pod uwage "Regularization Variance" !
-- cll :: Model -> [Sentence] -> Double
-- cll crf dataset = sum [prob crf sent | sent <- dataset]

-- prob2 :: SentR s => Model -> ProbArray -> ProbArray -> s
--       -> Int -> Lb -> Lb -> Double
-- prob2 crf alpha beta sent k x y
--     = alpha (k - 1) y + beta (k + 1) x
--     + phi crf (observationsOn sent k) a b
--     - zxBeta beta
--   where
--     a = interp sent k       x
--     b = interp sent (k - 1) y

prob2 crf alpha beta sent k psiMem x y ix
    = alpha (k - 1) y + beta (k + 1) x + psiMem x
    -- + onTransition crf a b - zxBeta beta
    + values crf ! ix - zxBeta beta

-- prob1 :: SentR s => Model -> ProbArray -> ProbArray
--       -> s -> Int -> Label -> Double
-- prob1 crf alpha beta sent k x = logSum
--     [ prob2 crf alpha beta sent k x y
--     | y <- interpIxs sent (k - 1) ]

prob1 :: Sent s => Model -> ProbArray -> ProbArray -> s -> Int -> Lb -> Double
prob1 crf alpha beta sent k x =
    alpha k x + beta (k + 1) x - zxBeta beta

expectedFeaturesOn :: Sent s => Model -> ProbArray -> ProbArray -> s
                   -> Int -> [(FeatIx, Double)]
expectedFeaturesOn crf alpha beta sent k =
    tFeats ++ oFeats
  where
    psiMem = computePsiMem crf sent k
    pr1 = prob1 crf alpha beta sent k
    pr2 = prob2 crf alpha beta sent k psiMem

    oFeats = [ (ix, pr1 x) 
             | o <- sent `obsOn` k
             , (x, ix) <- L.toList $ obIxs crf ! o ]

    tFeats
        | k == 0 = 
            [ (ix, pr1 x) 
            | (x, ix) <- zip [0..] $ L.toList $ sgIxs crf ]
        | otherwise =
            [ (ix, pr2 x y ix) 
            | x <- [0 .. lbNum crf - 1]
            , (y, ix) <- L.toList $ prevIxs crf ! x ]

expectedFeaturesIn :: Sent s => Model -> s -> [(FeatIx, Double)]
expectedFeaturesIn crf sent =
    -- force parallel computation of alpha and beta tables
    -- zx1 `par` zx2 `pseq` zx1 `pseq` trace (show (zx1, zx2, zx2-zx1)) $ concat
    zx1 `par` zx2 `pseq` zx1 `pseq` concat
    ( [ expectedFeaturesOn crf alpha beta sent k
      | k <- [0 .. sentLen sent - 1] ]
      -- TODO: Remove parallel annotation? Does it improve performance?
      -- parallel computation on different positions
      `using` parList (evalList evalElem) )
    where alpha = forward logSum crf sent
          beta = backward logSum crf sent
          zx1 = zxAlpha sent alpha
          zx2 = zxBeta beta
          evalElem = evalTuple2 rseq rseq
