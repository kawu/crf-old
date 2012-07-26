module Data.CRF.RCRF.Infere
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

import           Control.Applicative ((<*>), (<$>))
import           Data.List (maximumBy)
import           Data.Function (on)
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import           Control.Parallel.Strategies (rseq, parMap)
import           Control.Parallel (par, pseq)
import           GHC.Conc (numCapabilities)

import qualified Data.CRF.Control.DynamicProgramming as DP
import           Data.CRF.LogMath (logSum)
import           Data.CRF.Util (partition)

import Data.CRF.Base hiding (DataSet)
import Data.CRF.R
import Data.CRF.Y
import Data.CRF.Feature
import Data.CRF.RCRF.Model

type ProbArray = Int -> Lb -> Double
type AccF = [Double] -> Double

-- BEG Basic definitions.

lbsNum :: Model -> Sent R -> Int -> Int
lbsNum crf sent k
    | m > 0     = m
    | otherwise = lbNum crf
  where
    m = (U.length.unR) (sent V.! k)

-- | Label indices for a given position. If restriction set is empty,
-- set of all possible labels is taken (assuming that the first argument
-- is equall to a number of labels).
labelIxs :: Model -> Sent R -> Int -> [Int]
labelIxs crf sent k = [0 .. lbsNum crf sent k - 1]

labelOn :: R -> Int -> Lb
labelOn r k
    | m > 0     = v U.! k
    | otherwise = k
  where
    v = unR r
    m = U.length v

-- labelIxs2 :: Model -> Sent R -> Int -> [(Int, Int)]
-- labelIxs2 crf s 0 = error "labelIxs2: k == 0"
-- labelIxs2 crf s k = 
--     [ (i, j)
--     | i <- labelIxs crf s k
--     , j <- labelIxs crf s (k-1) ]
-- 
-- sgFeatFor :: Sent R -> Int -> Feature
-- sgFeatFor sent = SFeature . labelOn (sent V.! 0)
-- 
-- trFeatFor :: Sent R -> Int -> (Int, Int) -> Feature
-- trFeatFor _    0 _      = error "trFeatFor: k == 0"
-- trFeatFor sent k (a, b) =
--     TFeature x y
--   where
--     x = labelOn (sent V.! k) a
--     y = labelOn (sent V.! (k-1)) b
-- 
-- obFeatFor :: Sent R -> Int -> Int -> [Feature]
-- obFeatFor sent k a =
--     [ OFeature o x
--     | o <- obs (sent V.! k) ]
--   where
--     x = labelOn (sent V.! k) a

-- END Basic definitions.

{-# INLINE onWord #-}
onWord :: Model -> [Ob] -> Lb -> Double
onWord crf os x = sum
    [ onOFeat crf o x
    | o <- os ]

phi :: Model -> [Ob] -> Lb -> Lb -> Double
phi crf w x y = onWord crf w x + onTFeat crf x y

computePsi :: Model -> Sent R -> Int -> Int -> Double
computePsi crf sent i = (A.!) $ A.array bounds
    [ (k, psi crf x k)
    | k <- labelIxs crf sent i ]
  where
    psi crf x = onWord crf (obs x) . labelOn x
    bounds = (0, lbsNum crf sent i - 1)
    x = sent V.! i

forward :: AccF -> Model -> Sent R -> ProbArray
forward acc crf sent = alpha where
    alpha = DP.flexible2 (0, V.length sent) bounds
        (\t i -> withMem (computePsi crf sent i) t i)
    bounds i
        | i == V.length sent = (0, 0)
        | otherwise = (0, lbsNum crf sent i - 1)
    withMem psi alpha i j
        | i == 0 = psi j + onSFeat crf a
        | i == V.length sent = acc
            [ alpha (i-1) k
            | k <- labelIxs crf sent (i-1) ]
        | otherwise = acc
            [ alpha (i-1) k + psi j
            + onTFeat crf a (b k)
            | k <- labelIxs crf sent (i-1) ]
      where
        a = labelOn (sent V.! i) j
        b = labelOn (sent V.! (i-1))

backward :: AccF -> Model -> Sent R -> ProbArray
backward acc crf sent = beta where
    beta = DP.flexible2 (0, V.length sent) bounds
        (\t i -> withMem (computePsi crf sent i) t i)
    bounds i
        | i == 0    = (0, 0)
        | otherwise = (0, lbsNum crf sent (i-1) - 1)
    withMem psi beta i j
        | i == V.length sent = 0.0
        | i == 0    = acc
            [ beta (i+1) h + psi h
            + onSFeat crf (c h)
            | h <- labelIxs crf sent i ]
        | otherwise = acc
            [ beta (i+1) h + psi h
            + onTFeat crf (c h) a
            | h <- labelIxs crf sent i ]
      where
        a = labelOn (sent V.! (i-1)) j
        c = labelOn (sent V.! i)

zxBeta :: ProbArray -> Double
zxBeta beta = beta 0 0

zxAlpha :: Sent R -> ProbArray -> Double
zxAlpha sent alpha = alpha (V.length sent) 0

zx :: Model -> Sent R -> Double
zx crf = zxBeta . backward logSum crf

zx' :: Model -> Sent R -> Double
zx' crf sent = zxAlpha sent $ forward logSum crf sent

argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

dynamicTag :: Model -> Sent R -> [Int]
dynamicTag crf sent = collectMaxArg (0, 0) [] mem where
    mem = DP.flexible2 (0, V.length sent) bounds
        (\t i -> withMem (computePsi crf sent i) t i)
    bounds i
        | i == 0    = (0, 0)
        | otherwise = (0, lbsNum crf sent (i-1) - 1)
    withMem psi mem i j
        | i == V.length sent = (-1, 0.0)
        | i == 0    = argmax eval' $ labelIxs crf sent i
        | otherwise = argmax eval  $ labelIxs crf sent i
      where
        eval  h = (snd $ mem (i + 1) h) + psi h + onTFeat crf (c h) a
        eval' h = (snd $ mem (i + 1) h) + psi h + onSFeat crf (c h)
        a = labelOn (sent V.! (i-1)) j
        c = labelOn (sent V.! i)
    collectMaxArg (i, j) acc mem =
        collect (mem i j)
      where
        collect (h, _)
            | h == -1 = reverse acc
            | otherwise = collectMaxArg (i + 1, h) (h:acc) mem

tag' :: Model -> Sent R -> [Int]
tag' = dynamicTag

tag :: Model -> Sent R -> [Lb]
tag crf sent =
    let interp' (i, k) = labelOn (sent V.! i) k
    in  map interp' $ zip [0..] $ tag' crf sent

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

prob1 :: Model -> ProbArray -> ProbArray -> Sent R -> Int -> Int -> Double
prob1 crf alpha beta sent k i =
    alpha k i + beta (k+1) i - zxBeta beta

-- | NOTE: You have to add onTFeat potential.
-- TODO: Perhaps readability is more important and onTFeat should
-- be called here?
prob2 :: Model -> ProbArray -> ProbArray -> Sent R
      -> Int -> (Int -> Double) -> Int -> Int -> Double
prob2 crf alpha beta sent k psi i j =
    alpha (k-1) j + beta (k+1) i + psi i - zxBeta beta

expectedFeaturesOn
    :: Model -> ProbArray -> ProbArray -> Sent R
    -> Int -> [(Feature, Double)]
expectedFeaturesOn crf alpha beta sent k =
    oFeats ++ tFeats 
  where
    psi = computePsi crf sent k
    pr1 = prob1 crf alpha beta sent k
    pr2 = prob2 crf alpha beta sent k psi

    r  = sent V.! k
    r' = sent V.! (k-1)

    oFeats = [ (OFeature o x, p)
             | i <- labelIxs crf sent k
             , p <- [pr1 i]
             , x <- [labelOn r i]
             , o <- obs r ]

    tFeats
        | k == 0 = 
            [ (SFeature x, pr1 i)
            | i <- labelIxs crf sent k
            , x <- [labelOn r  i] ]
        | otherwise =
            [ (TFeature x y, pr2 i j + onTFeat crf x y)
            | i <- labelIxs crf sent k
            , j <- labelIxs crf sent (k-1)
            , x <- [labelOn r  i]
            , y <- [labelOn r' j] ]

expectedFeaturesIn :: Model -> Sent R -> [(Feature, Double)]
expectedFeaturesIn crf sent = zx `par` zx' `pseq` zx `pseq`
    concat [expectedOn k | k <- [0 .. V.length sent - 1] ]
  where
    expectedOn = expectedFeaturesOn crf alpha beta sent
    alpha = forward logSum crf sent
    beta = backward logSum crf sent
    zx  = zxAlpha sent alpha
    zx' = zxBeta beta
