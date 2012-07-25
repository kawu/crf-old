{-# LANGUAGE TupleSections #-}

module Data.CRF.R
( R (..)
, obs
, lbs
, lbOn
, safeLbOn
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

-- | Word with additional constraint on possible labels. 
data R = R
    { unX :: U.Vector Ob
    , unR :: U.Vector Lb }

{-# INLINE obs #-}
obs :: R -> [Ob]
obs = U.toList . unX

{-# INLINE lbs #-}
lbs :: R -> [Lb]
lbs = U.toList . unR

{-# INLINE lbOn #-}
lbOn :: R -> Int -> Lb
lbOn r k = unR r U.! k

{-# INLINE safeLbOn #-}
safeLbOn :: R -> Int -> Maybe Lb
safeLbOn r k = unR r U.!? k

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

toWord :: ToWord (R, Y)
toWord w = Word (obs (fst w)) (lbs (fst w)) (choice (snd w))
