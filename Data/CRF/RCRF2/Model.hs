module Data.CRF.RCRF2.Model
( Model (..)
, mkModel
-- , lbSet
, featToIx
, IxVect
, sgValue
-- , sgIxs
-- , obIxs
-- , nextIxs
-- , prevIxs
) where

import           Data.Maybe (fromJust)
import           Data.List (groupBy, sort)
import           Data.Function (on)
import qualified Data.Set as Set
import qualified Data.Map as M
-- import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import           Data.Binary
import           Data.Vector.Binary

import           SGD

import           Data.CRF.Base
import           Data.CRF.Feature
import           Data.CRF.LogMath (mInf)

noKey :: Int
noKey = -1

-- | List of (label, feature index) pairs.
type IxVect = U.Vector LbIx
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
    , sgIxs     :: U.Vector FeatIx
    -- | Set of acceptable labels when known value of the observation.
    , obIxs     :: V.Vector IxVect
    -- | Set of "previous" labels when known value of the current label.
    , prevIxs   :: V.Vector IxVect
    -- | Set of "next" labels when known value of the current label.
    , nextIxs   :: V.Vector IxVect }

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

-- | Construct CRF model from associations list.  There should be
-- no repetition of features in the input list.
fromList :: Int -> [(Feature, Double)] -> Model
fromList lbNum fs =
    let featLbs (SFeature x) = [x]
    	featLbs (OFeature _ x) = [x]
        featLbs (TFeature x y) = [x, y]
        featObs (OFeature o _) = [o]
        featObs _ = []

        ixMap = M.fromList $ zip (map fst fs) [0..]
    
        nub   = Set.toList . Set.fromList
        obSet = nub $ concatMap (featObs . fst) fs

        sFeats = [feat | (feat, val) <- fs, isSFeat feat]
        tFeats = [feat | (feat, val) <- fs, isTFeat feat]
        oFeats = [feat | (feat, val) <- fs, isOFeat feat]
        
        sgIxs = mkSg lbNum
            [ (x, featToIx' crf feat)
            | feat@(SFeature x) <- sFeats ]
        mkSg n xs = U.replicate n noKey U.// xs

        prevIxs = adjVects lbNum
            [ (x, (y, featToIx' crf feat))
            | feat@(TFeature x y) <- tFeats ]

        nextIxs = adjVects lbNum
            [ (y, (x, featToIx' crf feat))
            | feat@(TFeature x y) <- tFeats ]

        obIxs = adjVects (length obSet)
            [ (o, (x, featToIx' crf feat))
            | feat@(OFeature o x) <- oFeats ]

        -- | Adjacency vectors.
        adjVects n xs =
            init V.// update
          where
            init = V.replicate n (U.fromList [])
            update = map mkVect $ groupBy ((==) `on` fst) $ sort xs
            mkVect (x:xs) = (fst x, U.fromList $ sort $ map snd (x:xs))

        values =
            U.replicate (length fs) 0.0
          U.//
            [(featToIx' crf feat, val) | (feat, val) <- fs]

        featToIx' crf = fromJust . featToIx crf

        checkSet set cont =
            if set == [0 .. length set - 1]
                then cont
                else error "Internal.fromList: basic assumption not fulfilled"

        crf = Model values ixMap lbNum sgIxs obIxs prevIxs nextIxs
    in  checkSet obSet $ crf

mkModel :: Int -> [Feature] -> Model
mkModel lbNum fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList lbNum (zip fs' vs)

-- {-# INLINE lbSet #-}
-- lbSet :: Model -> [Lb]
-- lbSet crf = [0 .. lbNum crf - 1]

{-# INLINE featToIx #-}
featToIx :: Model -> Feature -> Maybe FeatIx
-- featToIx crf feat = ixMap crf M.! feat
featToIx crf feat = M.lookup feat (ixMap crf)

{-# INLINE sgValue #-}
sgValue :: Model -> Lb -> Double
sgValue crf x =
    if ix == noKey
    	-- | It makes huge difference, mInf or 0!
        then 0
        -- then mInf
        else values crf U.! ix
  where
    ix = sgIxs crf U.! x

-- sgIxs :: Model -> [LbIx]
-- {-# INLINE sgIxs #-}
-- sgIxs crf = 
--     filter (\(_, ix) -> ix >= 0) $ zip [0..] $ U.toList $ sgIxs crf
-- 
-- obIxs :: Model -> Ob -> [LbIx]
-- {-# INLINE obIxs #-}
-- obIxs crf ob = U.toList $ obIxs crf V.! ob
-- 
-- nextIxs :: Model -> Lb -> [LbIx]
-- {-# INLINE nextIxs #-}
-- nextIxs crf x = U.toList $ nextIxs crf V.! x
-- 
-- prevIxs :: Model -> Lb -> [LbIx]
-- {-# INLINE prevIxs #-}
-- prevIxs crf x = U.toList $ prevIxs crf V.! x
