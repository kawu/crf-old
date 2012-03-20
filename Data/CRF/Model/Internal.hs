module Data.CRF.Model.Internal
( Model (..)
-- , fromList
-- , toList
, mkModel
, featToIx
, FeatIx
) where

-- import Control.Monad (forM_)
-- import Data.Maybe (fromJust)
-- import Data.Ix (range, inRange, rangeSize)
-- 
-- import qualified Data.Array as A
-- import qualified Data.Array.IO as A
-- import qualified Data.Array.MArray as A
-- import qualified Data.Array.Unboxed as UA
-- import qualified Data.IntMap as IM
-- 
-- import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)
-- 
-- 
-- import Data.CRF.Types
-- import Data.CRF.Feature
-- import Data.CRF.LogMath

import           Data.List (groupBy, sort)
import           Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.ListLike as L
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import           Data.Binary

import           SGD

import           Data.CRF.Base
import           Data.CRF.Feature
import           Data.CRF.Vector.Binary

type FeatIx = Int
type LbIx   = (Lb, FeatIx)

-- | TODO: Safe ixs vectors construction.  Now, if labels set is
-- *not* of the [0..lbNum-1] form, everything will crash!
data Model = Model
    -- | Model values.
    { values    :: U.Vector Double
    -- | Indices map.
    , ixMap     :: M.Map Feature FeatIx
    -- | Number of labels.
    , lbNum 	:: Int
    -- | Singular feature indices.
    , sgIxs  	:: U.Vector FeatIx
    -- | Set of acceptable labels when known value of the observation.
    , obIxs     :: V.Vector (U.Vector LbIx)
    -- | Set of "previous" labels when known value of the current label.
    , prevIxs   :: V.Vector (U.Vector LbIx)
    -- | Set of "next" labels when known value of the current label.
    , nextIxs   :: V.Vector (U.Vector LbIx) }

instance ParamCore Model where

    unsafeConsume f xs crf = do
        values' <- unsafeConsume f xs $ values crf
        return $ crf { values = values' }

    unsafeMap f crf = do
        values' <- unsafeMap f $ values crf
        return $ crf { values = values' }

    size = size . values

instance Binary Model where
    put crf = do
        put $ values crf
        put $ ixMap crf
        put $ lbNum crf
        put $ sgIxs crf
        put $ obIxs crf
        put $ prevIxs crf
        put $ nextIxs crf
    get = do
        values <- get
        ixMap <- get
        lbNum <- get
        sgIxs <- get
        obIxs <- get
        prevIxs <- get
        nextIxs <- get
        return $ Model values ixMap lbNum sgIxs obIxs prevIxs nextIxs

-- instance Show Model where
--     show = unlines . map show . toList
-- 
-- toList :: Model -> [(Feature, Double)]
-- toList crf =
--     [ (TFeature x y, v)
--     | ((x, y), v) <- map dcd $ UA.assocs $ transIxs crf ]
--     ++
--     [ (OFeature o x, v)
--     | (o, om) <- A.assocs $ obvsIxs crf
--     , (x, v) <- map dcd $ IM.toList om ]
--   where
--     dcd (key, ix) = (key, values crf U.! ix)

-- | Construct CRF model from associations list.  There should be
-- no repetition of features in the input list.
fromList :: [(Feature, Double)] -> Model
fromList fs =
    let featLabels (SFeature x) = [x]
    	featLabels (OFeature _ x) = [x]
        featLabels (TFeature x y) = [x, y]
        featObs (OFeature o _) = [o]
        featObs _ = []

        ixMap = M.fromList $ zip (map fst fs) [0..]
    
        -- lbNum = (+1) $ maximum $ Set.toList $ Set.fromList
        --       $ concat $ map featLabels $ map fst fs
        lbNum = Set.size $ Set.fromList $ concatMap (featLabels . fst) fs

        sFeats = [feat | (feat, val) <- fs, isSFeat feat]
        tFeats = [feat | (feat, val) <- fs, isTFeat feat]
        oFeats = [feat | (feat, val) <- fs, isOFeat feat]
        
        sgIxs = L.fromList $ map snd $ sort
            [ (x, featToIx crf feat)
            | feat@(SFeature x) <- sFeats ]

        prevIxs = adjVects
            [ (x, (y, featToIx crf feat))
            | feat@(TFeature x y) <- tFeats ]

        nextIxs = adjVects
            [ (y, (x, featToIx crf feat))
            | feat@(TFeature x y) <- tFeats ]

        obIxs = adjVects
            [ (o, (x, featToIx crf feat))
            | feat@(OFeature o x) <- oFeats ]

        -- | Adjacency vectors.
        adjVects =
            L.fromList . map mkVect . groupBy ((==) `on` fst) . sort
          where
            mkVect = L.fromList . sort . map snd

        values =
            U.replicate (length fs) 0.0
          U.//
            [(featToIx crf feat, val) | (feat, val) <- fs]

        crf = Model values ixMap lbNum sgIxs obIxs prevIxs nextIxs
    in  crf

mkModel :: [Feature] -> Model
mkModel fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList (zip fs' vs)

featToIx :: Model -> Feature -> FeatIx
featToIx crf feat = ixMap crf M.! feat

-- onOFeat :: Obser -> Label -> Model -> Double
-- onOFeat o x crf =
--     case oFeatToIx o x crf of
--         Just ix -> values crf U.! ix
--         Nothing -> 0.0
-- 
-- onTFeat :: Label -> Label -> Model -> Double
-- onTFeat x y crf =
--     case tFeatToIx x y crf of
--         Just ix -> values crf U.! ix
--         Nothing -> 0.0
