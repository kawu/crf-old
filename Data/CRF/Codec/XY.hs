module Data.CRF.Codec.XY
( encode
) where

import Data.Maybe (catMaybes, fromJust)
import qualified Data.Vector.Unboxed as U

import Data.CRF.Codec
import qualified Data.CRF.Internal.XY as I
import qualified Data.CRF.XY as E

-- | TODO: keyDefault could be computed once for a codec.
encode :: Ord a => Codec a -> E.XY a -> I.XY
encode codec word =
    I.XY (U.fromList obs') (U.fromList choice')
  where
    obs'    = catMaybes $ map (encodeO codec) (E.obs word)
    choice' = [ (tryEncodeL y, prob)
              | (y, prob) <- E.choice word ]
    tryEncodeL = maybe keyDefault id . encodeL codec 
    keyDefault = fromJust $ encodeL codec $ lbDefault codec

-- | TODO: Implement encodeSent (for each configuration).
