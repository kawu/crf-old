{-# LANGUAGE FlexibleContexts #-}

module Data.CRF.CRF.Infere
( module Data.CRF.CRF.Model
-- , tag
-- -- , prob
-- -- , cll
-- -- , tagProbs
-- -- , tagProbs'
-- , accuracy
-- , expectedFeaturesIn
-- , zx
-- , zx'
) where

import           Data.List (maximumBy)
import           Data.Function (on)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import           Control.Parallel.Strategies (rseq, parMap)
import           Control.Parallel (par, pseq)
import           GHC.Conc (numCapabilities)

import qualified Data.CRF.Control.DynamicProgramming as DP
import           Data.CRF.Util (partition)
import           Data.CRF.LogMath (logSum, mInf)
import           Data.CRF.Base hiding (DataSet)
import           Data.CRF.X
import           Data.CRF.Y
import           Data.CRF.Feature
import           Data.CRF.CRF.Model

type ProbArray = Int -> Lb -> Double
type AccF = [Double] -> Double

-- (!) :: L.ListLike full item => full -> Int -> item 
-- (!) = L.index

-- | TODO: Try optimizing with SPECIALIZE pragma! 

-- | General model methods.

-- | TODO: It may me faster with Data.Map (but not in general case).
-- Or we can choose data structure depending on the restricted set (size)?
-- Perhaps adding better memory managment would be enough.
computePsi :: Model -> Sent X -> Int -> Lb -> Double
computePsi crf sent i = (A.!) $ A.accumArray (+) 0.0 bounds
    [ (lb, values crf U.! ix)
    | ob <- obs (sent V.! i)
    , (lb, ix) <- obIxs crf ob ]
  where
    -- | TODO: better memory managment? We could, also,
    -- operate on label indices and use an array of
    -- restricted set size...
    bounds = (0, lbNum crf - 1)

-- | Forward table computation.
forward :: AccF -> Model -> Sent X -> ProbArray
forward acc crf sent =
    alpha
  where
    alpha = DP.flexible2 (0, V.length sent) wordBounds
                (\t k -> withMem (computePsi crf sent k) t k)

    wordBounds k
        | k == V.length sent = (0, 0)
        | otherwise = (0, lbNum crf - 1)

    -- | Forward table equation, where k is current position, x is a label
    -- on current position and psi is a psi table computed for current
    -- position.
    -- FIXME: null sentence?
    withMem psi alpha k x
        | k == 0 = psi x + sgValue crf x 
        | k == V.length sent = acc
            [ alpha (k - 1) y
            | y <- lbSet crf ]
        | otherwise = acc
            [ alpha (k - 1) y + psi x + values crf U.! ix
            | (y, ix) <- prevIxs crf x ]

-- | Backward table computation.
backward :: AccF -> Model -> Sent X -> ProbArray
backward acc crf sent =
    beta
  where
    beta = DP.flexible2 (0, V.length sent) wordBounds
               (\t k -> withMem (computePsi crf sent k) t k)

    wordBounds k
        | k == 0    = (0, 0)
        | otherwise = (0, lbNum crf - 1)

    -- | Backward table equation, where k is current position, y is a label
    -- on previous, k-1, position and psi is a psi table computed for current
    -- position.
    withMem psi beta k y
        | k == V.length sent = 0.0
        | k == 0    = acc
            [ beta (k + 1) x + psi x + values crf U.! ix
            | (x, ix) <- sgIxs crf ]
        | otherwise = acc
            [ beta (k + 1) x + psi x + values crf U.! ix
            | (x, ix) <- nextIxs crf y ]

zxBeta :: ProbArray -> Double
zxBeta beta = beta 0 0

zxAlpha :: Sent X -> ProbArray -> Double
zxAlpha sent alpha = alpha (V.length sent) 0

zx :: Model -> Sent X -> Double
zx crf = zxBeta . backward logSum crf

zx' :: Model -> Sent X -> Double
zx' crf sent = zxAlpha sent $ forward logSum crf sent

--------------------------------------------------------------
argmax :: (Ord b) => (a -> b) -> [a] -> Maybe (a, b)
argmax f [] = Nothing
argmax f xs =
    Just $ foldl1 choice $ map (\x -> (x, f x)) xs
  where
    choice (x1, v1) (x2, v2)
        | v1 > v2 = (x1, v1)
        | otherwise = (x2, v2)

dynamicTag :: Model -> Sent X -> [Lb]
dynamicTag crf sent =
    collectMaxArg (0, 0) [] mem
  where
    mem = DP.flexible2 (0, V.length sent) wordBounds
               (\t k -> withMem (computePsi crf sent k) t k)

    wordBounds k
        | k == 0    = (0, 0)
        | otherwise = (0, lbNum crf - 1)

    withMem psiMem mem k y
        | k == V.length sent = (-1, 0.0)
        | k == 0    = prune $ argmax eval $ sgIxs crf
        | otherwise = prune $ argmax eval $ nextIxs crf y
      where
        eval (x, ix) = (snd $ mem (k + 1) x) + psiMem x + values crf U.! ix
        prune (Just ((x, ix), v)) = (x, v)
        prune Nothing = (-1, mInf)

    collectMaxArg (i, j) acc mem =
        collect $ mem i j
      where
        collect (h, _)
            | h == -1 = reverse acc
            | otherwise = collectMaxArg (i + 1, h) (h:acc) mem

tag :: Model -> Sent X -> [Lb]
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
--         | i <- [0 .. V.length sent - 1] ]
-- 
-- -- tag probabilities with respect to
-- -- marginal distributions
-- tagProbs' :: Sent s => Model -> s -> [[Double]]
-- tagProbs' crf sent =
--     let alpha = forward logSum crf sent
--         beta = backward logSum crf sent
--     in  [ [ exp $ prob1 crf alpha beta sent i k
--           | k <- interpIxs sent i ]
--         | i <- [0 .. V.length sent - 1] ]

goodAndBad :: Model -> Sent X -> Sent Y -> (Int, Int)
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

type DataSet = [(Sent X, Sent Y)]

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

--------------------------------------------------------------

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
    + values crf U.! ix - zxBeta beta

-- prob1 :: SentR s => Model -> ProbArray -> ProbArray
--       -> s -> Int -> Label -> Double
-- prob1 crf alpha beta sent k x = logSum
--     [ prob2 crf alpha beta sent k x y
--     | y <- interpIxs sent (k - 1) ]

prob1 :: Model -> ProbArray -> ProbArray -> Sent X -> Int -> Lb -> Double
prob1 crf alpha beta sent k x =
    alpha k x + beta (k + 1) x - zxBeta beta

expectedFeaturesOn :: Model -> ProbArray -> ProbArray -> Sent X
                   -> Int -> [(FeatIx, Double)]
expectedFeaturesOn crf alpha beta sent k =
    tFeats ++ oFeats
  where
    psiMem = computePsi crf sent k
    pr1 = prob1 crf alpha beta sent k
    pr2 = prob2 crf alpha beta sent k psiMem

    oFeats = [ (ix, pr1 x) 
             | o <- obs (sent V.! k)
             , (x, ix) <- obIxs crf o ]

    tFeats
        | k == 0 = 
            [ (ix, pr1 x) 
            | (x, ix) <- sgIxs crf ]
        | otherwise =
            [ (ix, pr2 x y ix) 
            | x <- lbSet crf
            , (y, ix) <- prevIxs crf x ]

expectedFeaturesIn :: Model -> Sent X -> [(FeatIx, Double)]
expectedFeaturesIn crf sent = zx `par` zx' `pseq` zx `pseq`
    concat [expectedOn k | k <- [0 .. V.length sent - 1] ]
  where
    expectedOn = expectedFeaturesOn crf alpha beta sent
    alpha = forward logSum crf sent
    beta = backward logSum crf sent
    zx  = zxAlpha sent alpha
    zx' = zxBeta beta
