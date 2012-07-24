module Data.CRF.Codec.X
( encode
) where

import Data.Maybe (catMaybes)
import qualified Data.Vector.Unboxed as U

import Data.CRF.Codec
import qualified Data.CRF.Internal.X as I
import qualified Data.CRF.X as E

encode :: Ord a => Codec a -> E.X a -> I.X
encode codec word
    = I.X . U.fromList . catMaybes
    $ map (encodeO codec) (E.obs word)
