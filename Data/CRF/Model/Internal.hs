module Data.CRF.Model.Internal
( FeatIx
, Model (..)
-- , fromList
-- , toList
, mkModel
, lbSet
, featToIx
, sgValue
, sgIxs
, obIxs
, nextIxs
, prevIxs
) where

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
import           Data.CRF.LogMath (mInf)

type FeatIx = Int
type LbIx   = (Lb, FeatIx)

data Model = Model
    -- | Model values.
    { values    :: U.Vector Double
    -- | Indices map.
    , ixMap     :: M.Map Feature FeatIx
    -- | Number of labels.
    , lbNum 	:: Int
    -- | Singular feature indices.  Index is equall to -1 if feature
    -- is not present in the model.
    , sgIxsV 	:: U.Vector FeatIx
    -- | Set of acceptable labels when known value of the observation.
    , obIxsV    :: V.Vector (U.Vector LbIx)
    -- | Set of "previous" labels when known value of the current label.
    , prevIxsV  :: V.Vector (U.Vector LbIx)
    -- | Set of "next" labels when known value of the current label.
    , nextIxsV  :: V.Vector (U.Vector LbIx) }

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
        put $ sgIxsV crf
        put $ obIxsV crf
        put $ prevIxsV crf
        put $ nextIxsV crf
    get = do
        values <- get
        ixMap <- get
        lbNum <- get
        sgIxsV <- get
        obIxsV <- get
        prevIxsV <- get
        nextIxsV <- get
        return $ Model values ixMap lbNum sgIxsV obIxsV prevIxsV nextIxsV

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
    let featLbs (SFeature x) = [x]
    	featLbs (OFeature _ x) = [x]
        featLbs (TFeature x y) = [x, y]
        featObs (OFeature o _) = [o]
        featObs _ = []

        ixMap = M.fromList $ zip (map fst fs) [0..]
    
        nub   = Set.toList . Set.fromList
        obSet = nub $ concatMap (featObs . fst) fs
        lbSet = nub $ concatMap (featLbs . fst) fs
        lbNum = length lbSet

        sFeats = [feat | (feat, val) <- fs, isSFeat feat]
        tFeats = [feat | (feat, val) <- fs, isTFeat feat]
        oFeats = [feat | (feat, val) <- fs, isOFeat feat]
        
        sgIxsV = sgVects lbSet
            [ (x, featToIx crf feat)
            | feat@(SFeature x) <- sFeats ]

        prevIxsV = adjVects lbSet
            [ (x, (y, featToIx crf feat))
            | feat@(TFeature x y) <- tFeats ]

        nextIxsV = adjVects lbSet
            [ (y, (x, featToIx crf feat))
            | feat@(TFeature x y) <- tFeats ]

        obIxsV = adjVects obSet
            [ (o, (x, featToIx crf feat))
            | feat@(OFeature o x) <- oFeats ]

        -- | Adjacency vectors.
        adjVects keys xs =
            init V.// update
          where
            init = L.replicate (length keys) (L.fromList [])
            update = map mkVect $ groupBy ((==) `on` fst) $ sort xs
            mkVect (x:xs) = (fst x, L.fromList $ sort $ map snd (x:xs))

        sgVects keys xs = L.replicate (length keys) (-1) U.// xs

        values =
            U.replicate (length fs) 0.0
          U.//
            [(featToIx crf feat, val) | (feat, val) <- fs]

        checkSet set cont =
            if set == [0 .. length set - 1]
                then cont
                else error "Internal.fromList: basic assumption not fulfilled"

        crf = Model values ixMap lbNum sgIxsV obIxsV prevIxsV nextIxsV
    in  checkSet lbSet $ checkSet obSet $ crf

mkModel :: [Feature] -> Model
mkModel fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList (zip fs' vs)

lbSet :: Model -> [Lb]
lbSet crf = [0 .. lbNum crf - 1]

featToIx :: Model -> Feature -> FeatIx
featToIx crf feat = ixMap crf M.! feat

sgValue :: Model -> Lb -> Double
sgValue crf x =
    case sgIxsV crf U.! x of
        -1 -> mInf
        ix -> values crf U.! ix

sgIxs :: Model -> [LbIx]
{-# INLINE sgIxs #-}
sgIxs crf = 
    filter (\(_, ix) -> ix >= 0) $ zip [0..] $ L.toList $ sgIxsV crf

obIxs :: Model -> Ob -> [LbIx]
{-# INLINE obIxs #-}
obIxs crf ob = U.toList $ obIxsV crf V.! ob

nextIxs :: Model -> Lb -> [LbIx]
{-# INLINE nextIxs #-}
nextIxs crf x = U.toList $ nextIxsV crf V.! x

prevIxs :: Model -> Lb -> [LbIx]
{-# INLINE prevIxs #-}
prevIxs crf x = U.toList $ prevIxsV crf V.! x
