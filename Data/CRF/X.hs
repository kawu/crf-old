{-# LANGUAGE TupleSections #-}

module Data.CRF.X
( X (..)
, obs
, encode
) where

import qualified Data.Vector.Unboxed as U
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import qualified Data.CRF.Codec as Codec
import qualified Data.CRF.Word as Word
import Data.CRF.Word (Word(Word), ToWord)
import Data.CRF.Base
import Data.CRF.Y

-- | Simple word represented by a list of its observations.
newtype X = X { unX :: U.Vector Ob }

{-# INLINE obs #-}
obs :: X -> [Ob]
obs = U.toList . unX

encode :: Ord a => Codec.Codec a -> Word a -> (X, Y)
encode codec word =
    ( X (U.fromList x)
    , Y (U.fromList y) )
  where
    x = catMaybes $ map (Codec.encodeO codec) (Word.obs word)
    y = catMaybes
        [ (,pr) <$> Codec.encodeL codec lb
        | (lb, pr) <- Word.choice word ]

toWord :: ToWord (X, Y)
toWord w = Word (obs (fst w)) [] (choice (snd w))
