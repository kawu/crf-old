{-# LANGUAGE TupleSections #-}

module Data.CRF.R
( R (unX, unR)
, Rs
, lbs
, lbOn
, safeLbOn

, fromX
, toX

, encode
, encodeSent
, encode'
, encodeSent'
) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import qualified Data.CRF.Codec as Codec
import qualified Data.CRF.Word as Word
import Data.CRF.Word (Word(Word))
import Data.CRF.Base
import Data.CRF.Y
import Data.CRF.X (X(X))

-- | Word with additional constraint on possible labels. Invariants:
-- * All chosen labels (see Data.CRF.Y) belong to this set.
-- * Set unR is a distinct and ascending one.
data R = R
    { unX :: U.Vector Ob
    , unR :: U.Vector Lb }
    deriving (Show, Read, Eq, Ord)

-- | Should we check, if rNull is given in ascending order?
mkR :: U.Vector Lb -> [Ob] -> [Lb] -> R
mkR rNull x [] = R (U.fromList x) rNull
mkR rNull x r  = R (U.fromList x) (U.fromList . S.toList . S.fromList $ r)

type Rs = Sent R

instance HasObs R where
    {-# INLINE obs #-}
    obs = U.toList . unX

{-# INLINE lbs #-}
lbs :: R -> [Lb]
lbs = U.toList . unR

-- instance HasLbs R where
--     {-# INLINE lbs #-}
--     lbs = U.toList . unR

{-# INLINE lbOn #-}
lbOn :: R -> Int -> Lb
lbOn r k = unR r U.! k

{-# INLINE safeLbOn #-}
safeLbOn :: R -> Int -> Maybe Lb
safeLbOn r k = unR r U.!? k

-- | Construct R from X given a set of all labels occuring in dataset. 
{-# INLINE fromX #-}
fromX :: U.Vector Lb -> X -> R
fromX r (X x) = R x r

-- | Construct X from R discarding the set of possible labels.
{-# INLINE toX #-}
toX :: R -> X
toX (R x _) = X x

encode :: (Ord a, Ord b) => Codec.Codec a b -> Word a b -> (R, Y)
encode = encode' (U.fromList [])

encodeSent :: (Ord a, Ord b) => Codec.Codec a b -> [Word a b] -> (Rs, Ys)
encodeSent = encodeSent' (U.fromList [])

-- | Generic way of word encoding: you can specify default restriction vector
-- for the case when the restriction set is null.  It can be handy when we
-- want to have some default set of labels (it could be achieved in a more
-- natural way, but this one is more efficient).
encode' :: (Ord a, Ord b) => U.Vector Lb
        -> Codec.Codec a b -> Word a b -> (R, Y)
encode' rNull codec word =
    ( mkR rNull x r
    , Y (U.fromList y) )
  where
    x = catMaybes $ map (Codec.encodeO codec) (Word.obs word)
    r = map (Codec.encodeL' codec) (Word.lbs word)
    y = catMaybes
        [ (  , pr) <$> Codec.encodeL codec lb
        | (lb, pr) <- Word.choice word ]

encodeSent' :: (Ord a, Ord b) => U.Vector Lb
            -> Codec.Codec a b
            -> [Word a b] -> (Rs, Ys)
encodeSent' rNull codec words =
    (V.fromList rs, V.fromList ys)
  where
    ps = map (encode' rNull codec) words
    rs = map fst ps
    ys = map snd ps
