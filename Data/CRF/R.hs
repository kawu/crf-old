{-# LANGUAGE TupleSections #-}

module Data.CRF.R
( R (..)
, Rs
, lbs
, lbOn
, safeLbOn

, fromX
, toX

, encode
, encodeSent
) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import qualified Data.CRF.Codec as Codec
import qualified Data.CRF.Word as Word
import Data.CRF.Word (Word(Word))
import Data.CRF.Base
import Data.CRF.Y
import Data.CRF.X (X(X))

-- | Word with additional constraint on possible labels. We assume, that
-- all chosen labels (see Data.CRF.Y) belong to this set.
data R = R
    { unX :: U.Vector Ob
    , unR :: U.Vector Lb }

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

encode :: Ord a => Codec.Codec a -> Word a -> (R, Y)
encode codec word =
    ( R (U.fromList x) (U.fromList r)
    , Y (U.fromList y) )
  where
    x = catMaybes $ map (Codec.encodeO codec) (Word.obs word)
    r = catMaybes $ map (Codec.encodeL codec) (Word.lbs word)
    y = catMaybes
        [ (,pr) <$> Codec.encodeL codec lb
        | (lb, pr) <- Word.choice word ]

encodeSent :: Ord a => Codec.Codec a -> [Word a] -> (Rs, Ys)
encodeSent codec words =
    (V.fromList rs, V.fromList ys)
  where
    ps = map (encode codec) words
    rs = map fst ps
    ys = map snd ps
