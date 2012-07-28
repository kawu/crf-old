{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Data.CRF.RCRF2.Intersect
( intersect
) where

import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Control.Applicative ((<$>))
-- import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as U

import Data.CRF.Base (Lb, FeatIx)

import Debug.Trace (trace)

-- checkAsc :: Ord a => [a] -> ()
-- checkAsc (x:y:xs)
--     | x < y     = checkAsc xs
--     | otherwise = error "checkAsc: argument not an ascending list"
-- checkAsc _ = ()

-- intersect
--     :: IM.IntMap FeatIx -- ^ Map from a label to feature index
--     -> [(Int, Lb)]      -- ^ Label index and the label itself
--     -> [(Int, FeatIx)]  -- ^ Label index and feature index
-- intersect ixMap xs = catMaybes
--     [ (k,) <$> IM.lookup x ixMap
--     | (k, x) <- xs ]

-- -- | Assumption: both input list are given in an ascending order.
-- intersect
--     :: U.Vector (Lb, FeatIx)    -- ^ Vector of (label, features index) pairs
--     -> U.Vector Lb              -- ^ Vector of labels
--     -- | Intersection of arguments: vector indices from the second list
--     -- and feature indices from the first list.
--     -> [(Int, FeatIx)]
-- intersect xs ys = merge
--     (zip (U.toList ys) [0..])
--     (U.toList xs)

-- | Assumption: both input list are given in an ascending order.
intersect
    :: U.Vector (Lb, FeatIx)    -- ^ Vector of (label, features index) pairs
    -> U.Vector Lb              -- ^ Vector of labels
    -- | Intersection of arguments: vector indices from the second list
    -- and feature indices from the first list.
    -> [(Int, FeatIx)]
intersect xs ys
    | n == 0 || m == 0 = []
--     | n + m < 25 = merge xs ys
--     | n < m `quot` 5 = searchMany' xs ys
--     | n < m `quot` 5 =
--         let bs x = searchBy fst x xs
--         in  searchMany bs n f m g
--     | m < n `quot` 10 =
--         let bs y = searchBy id y ys
--             swap (x, y) = (y, x)
--         in  map swap $ searchMany bs m g n f
    | otherwise = merge xs ys
  where
    n = U.length ys
    m = U.length xs
    f k = (ys `U.unsafeIndex` k, k)
    g = (xs `U.unsafeIndex`)

merge :: U.Vector (Lb, FeatIx) -> U.Vector Lb -> [(Int, FeatIx)]
merge xs ys = doIt 0 0
  where
    m = U.length xs
    n = U.length ys
    doIt i j
        | i >= m || j >= n = []
        | otherwise = case compare x y of
            EQ -> (j, ix) : doIt (i+1) (j+1)
            LT -> doIt (i+1) j
            GT -> doIt i (j+1)
      where
        (x, ix) = xs `U.unsafeIndex` i
        y = ys `U.unsafeIndex` j

searchMany
    :: (Lb -> Int -> Int -> Int)    -- ^ Binary search with respect to w
    -> Int -> (Int -> (Lb, a))      -- ^ Vector v (size and body)
    -> Int -> (Int -> (Lb, b))      -- ^ Vector w (size and body)
    -> [(a, b)]
searchMany bs n v m w = doIt 0 n 0 m
  where
    doIt !p !q !p' !q'
        | p >= q    = []
        | p' >= q'  = []
        | j == q'   = doIt p i p' j
        | x == y    = (a, b)
                    : doIt p i p' j
                   ++ doIt (i+1) q (j+1) q'
        | otherwise = doIt p i p' j
                   ++ doIt (i+1) q j q'
      where
        n = q - p
        i = p + (n `quot` 2)
        (x, a) = v i
        j = bs x p' q'
        (y, b) = w j

-- | Binary search for an element within [p, q) range. 
searchBy :: U.Unbox a => (a -> Lb) -> Lb
         -> U.Vector a -> Int -> Int -> Int
searchBy toLb x xs p q = doIt p q
  where
    doIt !p !q
        | n < 3 = case U.findIndex ((>=x).toLb) (U.unsafeSlice p n xs) of
            Just i  -> p + i
            Nothing -> q
        | otherwise = case compare x y of
            EQ -> k
            LT -> doIt p k
            GT -> doIt (k+1) q
      where
        n = q - p
        k = p + (n `quot` 2)
        y = toLb (xs `U.unsafeIndex` k)

-- | Simpler solution with binary search.
searchMany' :: U.Vector (Lb, FeatIx) -> U.Vector Lb -> [(Int, FeatIx)]
searchMany' xs ys = catMaybes
    [ (k,) <$> search x xs 
    | (k, x) <- zip [0..] (U.toList ys) ]

-- | Binary search for an element. 
search :: Lb -> U.Vector (Lb, FeatIx) -> Maybe FeatIx
search x xs = doIt 0 (U.length xs)
  where
    valueAt = snd . (xs `U.unsafeIndex`)
    lbAt = fst . (xs `U.unsafeIndex`)
    doIt !p !q
        | n > 2 = let k = p + (n `quot` 2) in
            case compare x (lbAt k) of
                EQ -> Just (valueAt k)
                LT -> doIt p k
                GT -> doIt (k+1) q
        | n == 2 = case compare x (lbAt p) of
            EQ -> Just (valueAt p)
            LT -> Nothing
            GT -> doIt (p+1) q
        | n == 1 = case compare x (lbAt p) of
            EQ -> Just (valueAt p)
            _  -> Nothing
        | n == 0 = Nothing
      where
        n = q - p
