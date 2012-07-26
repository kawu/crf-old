{-# LANGUAGE TupleSections #-}

module Data.CRF.X
( X (..)
, Xs
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

-- | Simple word represented by a list of its observations.
newtype X = X { unX :: U.Vector Ob }

type Xs = Sent X

instance HasObs X where
    {-# INLINE obs #-}
    obs = U.toList . unX

-- instance HasLbs X where
--     {-# INLINE lbs #-}
--     lbs = const []

encode :: (Ord a, Ord b) => Codec.Codec a b -> Word a b -> (X, Y)
encode codec word =
    ( X (U.fromList x)
    , Y (U.fromList y) )
  where
    x = catMaybes $ map (Codec.encodeO codec) (Word.obs word)
    y = catMaybes
        [ (,pr) <$> Codec.encodeL codec lb
        | (lb, pr) <- Word.choice word ]

encodeSent :: (Ord a, Ord b) => Codec.Codec a b -> [Word a b] -> (Xs, Ys)
encodeSent codec words =
    (V.fromList xs, V.fromList ys)
  where
    ps = map (encode codec) words
    xs = map fst ps
    ys = map snd ps
