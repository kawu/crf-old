{-# LANGUAGE TupleSections #-}

module Data.CRF.RCRF2.Intersect
( intersect
) where

import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
import qualified Data.IntMap as IM

import Data.CRF.Base (Lb, FeatIx)

import Debug.Trace (trace)

checkAsc :: Ord a => [a] -> ()
checkAsc (x:y:xs)
    | x < y     = checkAsc xs
    | otherwise = error "checkAsc: argument not an ascending list"
checkAsc _ = ()

intersect
    :: IM.IntMap FeatIx -- ^ Map from a label to feature index
    -> [(Int, Lb)]      -- ^ Label index and the label itself
    -> [(Int, FeatIx)]  -- ^ Label index and feature index
intersect ixMap xs = catMaybes
    [ (k,) <$> IM.lookup x ixMap
    | (k, x) <- xs ]

-- intersect
--     :: IM.IntMap FeatIx -- ^ Map from a label to feature index
--     -> [(Int, Lb)]      -- ^ Label index and the label itself
--     -> [(Int, FeatIx)]  -- ^ Label index and feature index
-- intersect ftMap xs =
--     let lbMap = IM.fromAscList [(y, x) | (x, y) <- xs]
--     in  checkAsc (map snd xs) `seq`
--         IM.elems $ IM.intersectionWith (,) lbMap ftMap

data Tree a b
    = Tree
        { key   :: !a
        , val   :: !b
        , left  :: !(Tree a b)
        , right :: !(Tree a b) }
    | Empty

keys :: Tree a b -> [a]
keys Empty          = []
keys (Tree x _ l r) = x : keys l ++ keys r

assocs :: Tree a b -> [(a, b)]
assocs (Tree x v l r) = (x, v) : assocs l ++ assocs r
assocs Empty = []

size :: Tree a b -> Int
size Empty          = 0
size (Tree x _ l r) = 1 + size l + size r

depth :: Tree a b -> Int
depth Empty          = 0
depth (Tree x _ l r) = 1 + max (depth l) (depth r)

checkTree :: Ord a => Tree a b -> Bool
checkTree Empty = True
checkTree (Tree x v l r)
    =  checkTree l
    && checkTree r
    && all (<x) (keys l)
    && all (>x) (keys r)

-- | Make balanced binary search tree from distinct, ascending list.
mkTree :: Ord a => [(a, b)] -> Tree a b
mkTree xs = tree
--     -- checkAsc (map fst xs) `seq` tree
--     | checkTree tree == False = error "tree corrupted"
--     | otherwise = trace ("tree OK: " ++ show (size tree, depth tree)) tree
  where
    (tree, []) = doIt (length xs) xs
    doIt n xs0
        | n > 2  =
            let k  = n `quot` 2 -- ^ Size of the left subtree
                k' = n - k - 1  -- ^ Size of the right subtree
                (left, xs1)  = doIt k  xs0
                ((x, v):xs2) = xs1
                (right, xs3) = doIt k' xs2
            in  (Tree x v left right, xs3)
        | n == 2 =
            let ((x, v):(y, w):xs1) = xs0
            in  (Tree x v Empty (Tree y w Empty Empty), xs1)
        | n == 1 =
            let ((x, v):xs1) = xs0
            in  (Tree x v Empty Empty, xs1)

searchMany :: Tree Lb Int -> IM.IntMap FeatIx -> [(Int, FeatIx)]
searchMany (Tree x k lt rt) ftMap
    | IM.null ftMap = []
    | otherwise = case ix' of
        Just ix -> (k, ix) : next 
        Nothing -> next
  where
    (lm, ix', rm) = IM.splitLookup x ftMap
    next = searchMany lt lm ++ searchMany rt rm
searchMany Empty _ = []

-- searchMany :: Tree Lb Int -> IM.IntMap FeatIx -> [(Int, FeatIx)]
-- searchMany (Tree x k lt rt) ftMap = case ix' of
--     Just ix -> (k, ix) : next 
--     Nothing -> next
--   where
--     ix' = IM.lookup x ftMap
--     next = searchMany lt ftMap ++ searchMany rt ftMap
-- searchMany Empty _ = []

-- -- | Assumption: labels (in [(Int, Lb)] list) are given in
-- -- an ascending order.
-- intersect
--     :: IM.IntMap FeatIx -- ^ Map from a label to feature index
--     -> [(Int, Lb)]      -- ^ Label index and the label itself
--     -> [(Int, FeatIx)]  -- ^ Label index and feature index
-- intersect ftMap xs =
--     let lbTree = mkTree [(y, x) | (x, y) <- xs]
--     in  searchMany lbTree ftMap
