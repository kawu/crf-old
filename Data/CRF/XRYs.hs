{-# LANGUAGE TypeSynonymInstances #-}

module Data.CRF.XRYs
( Xs (..)
, Rs (..)
, Ys (..)
, XRs (..)
, XRYs (..)
, Config (..)
, mkConfig
, mkXRs
, mkXRYs
) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Binary (Binary, put, get)
import qualified Data.Vector as V hiding (fromList, toList)
import Data.ListLike (fromList, toList, length, index, ListLike)

import Data.CRF.Types
import Data.CRF.Const
import qualified Data.CRF.Codec as C
import qualified Data.CRF.InOut as I

-- | Sentence -- sequence of words.
type Xs = V.Vector X

-- | Seqence of restrictions on label sets (possible interpretations).
type Rs = V.Vector R

-- | Sequence of label sets, each label with corresponding probability.
type Ys = V.Vector Y

-- | Sentence with restrictions (possible interpretations)
type XRs = (Xs, Rs)

-- | Sentence with restrictions and chosen interpretations
-- ( together with corresponding probabilitites ).
type XRYs = (Xs, Rs, Ys)

instance Sent Xs where
    observationsOn xs k
        | k < 0 || k >= length xs   = fromList []
        | otherwise                 = index xs k
    sentLen = length

instance Sent XRs where
    observationsOn (xs, _) = observationsOn xs
    sentLen (xs, _) = sentLen xs

instance SentR XRs where
    interpsOn (_, rs) k
        | k < 0 || k >= length rs   = fromList [dummy]
        | otherwise                 = index rs k

instance Sent XRYs where
    observationsOn (xs, _,  _) = observationsOn xs
    sentLen (xs, _, _) = sentLen xs

instance SentR XRYs where
    interpsOn (xs, rs, _) = interpsOn (xs, rs)

instance SentM XRYs where
    choiceOn (_, _, ys) k
        | k < 0 || k >= length ys   = fromList [(dummy, 1.0)]
        | otherwise                 = index ys k

-- -- TODO: dodac w ponizszych asercje:
-- -- * rs \subseteq allLabels
-- -- * ys \subseteq allLabels
-- -- * ys \subseteq rs
-- -- * !WAZNE! allLabels \subset [1..] (0 stanowi specjalną etykiete 'dummy';
-- --   można zmienić założenia, tak żeby normalne etykiety zaczynaly sie od 0)
-- -- * Analogicznie, obvs \subset [1..] 
-- 
-- -- | Assumption: lMin = 1, oMin = 1
-- data Config = Config
--     { allLabels :: R  -- Set of all labels
--     , lMax :: Int       -- Maximal label id
--     , oMax :: Int }     -- Maximal observation id
-- 
-- instance Binary Config where
--     put cfg = do
--         put $ toList $ allLabels cfg
--         put $ lMax cfg
--         put $ oMax cfg
--     get = do
--         allLabels <- fromList <$> get
--         lMax <- get
--         oMax <- get
--         return Config { allLabels = allLabels, lMax = lMax, oMax = oMax }
-- 
-- mkConfig :: C.Codec Int -> IO Config
-- mkConfig codec = do
--     when (lMin < 1) (fail "lMin < 1")
--     when (oMin < 1) (fail "oMin < 1")
--     return config
--   where
--     labels = M.keys $ C.lMap codec
--     observations = M.keys $ C.oMap codec
--     lMin = minimum labels
--     oMin = minimum observations
--     config = Config
--         { allLabels = fromList $ labels
--         , lMax = maximum labels
--         , oMax = maximum observations }
-- 
-- mkXRs :: Config -> I.SentR Int -> XRs
-- mkXRs cfg sent = (xs, rs)
--   where
--     xs = fromList2
--         [ filterO obvs
--         | I.WordR obvs _ <- sent ]
--     rs = fromList
--         [ if null labels
--             then allLabels cfg
--             else fromList $ filterL labels
--         | I.WordR _ labels <- sent ]
-- 
--     -- TODO: wartosc 0 zakodowana na sztywno ! da sie cos z tym zrobic ?
--     filterO = filter $ \x -> x < oMax cfg && x > 0
--     filterL = map $ \x -> if x < lMax cfg && x > 0
--         then unknown
--         else x
-- 
-- mkXRYs :: Config -> I.SentRM Int -> XRYs
-- mkXRYs cfg sent = (xs, rs, ys)
--   where
--     xs = fromList2
--         [ filterO obvs
--         | I.WordRM obvs _ _ <- sent ]
--     rs = fromList
--         [ if null labels
--             then allLabels cfg
--             else fromList $ filterL labels
--         | I.WordRM _ labels _ <- sent ]
--     ys = fromList2 [choices | I.WordRM _ _ choices <- sent]
-- 
--     -- TODO: wartosc 0 zakodowana na sztywno ! da sie cos z tym zrobic ?
--     filterO = filter $ \x -> x < oMax cfg && x > 0
--     filterL = map $ \x -> if x < lMax cfg && x > 0
--         then unknown
--         else x
-- 
-- fromList2 :: (ListLike w v, ListLike v x) => [[x]] -> w
-- fromList2 = fromList . map fromList
