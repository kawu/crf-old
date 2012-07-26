module Data.CRF.RCRF.Model
( Model (..)
, fromList
, mkModel
, lbSet
, featToIx
, oFeatToIx
, sFeatToIx
, tFeatToIx
, onOFeat
, onSFeat
, onTFeat
) where

import		 Control.Applicative ((<$>), (<*>))
import qualified Data.Array.Unboxed as A
import qualified Data.Array.IArray as A
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import           Data.Binary
import           Data.Vector.Binary
import           Data.List (sort, groupBy)
import           Data.Function (on)
import           Data.Maybe (fromJust)

import           SGD

import Data.CRF.Base
import Data.CRF.Feature

noKey :: Int
noKey = -1

data Model = Model
    -- | Model values.
    { values    :: U.Vector Double
    -- | For each label, a map from observations to feature indinces.
    , obIxs     :: V.Vector (IM.IntMap FeatIx)
    -- | Array of singular feature indices.
    , sgIxs     :: U.Vector FeatIx
    -- | Array of transition feature indices.
    , trIxs     :: A.UArray (Int, Int) FeatIx
    -- | Number of labels.
    , lbNum     :: Int }

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
        put $ obIxs crf
        put $ sgIxs crf
        put $ trIxs crf
        put $ lbNum crf
    get = Model <$> get <*> get <*> get <*> get <*> get

-- | Construct CRF model from associations list.  There should be
-- no repetition of features in the input list.
fromList :: [(Feature, Double)] -> Model
fromList fs =
    checkSet lbSet $ checkSet obSet $ crf
  where 
    crf = Model values obIxs sgIxs trIxs lbNum

    featLbs (SFeature x) = [x]
    featLbs (OFeature _ x) = [x]
    featLbs (TFeature x y) = [x, y]
    featObs (OFeature o _) = [o]
    featObs _ = []

    -- | We need it as a replacement for featToIx, which
    -- can be used only after a model has been constructed.
    ixMap = M.fromList $ zip (map fst fs) [0..]
    
    nub   = S.toList . S.fromList
    obSet = nub $ concatMap (featObs . fst) fs
    lbSet = nub $ concatMap (featLbs . fst) fs
    lbNum = length lbSet

    oFeats = [feat | (feat, val) <- fs, isOFeat feat]
    sFeats = [feat | (feat, val) <- fs, isSFeat feat]
    tFeats = [feat | (feat, val) <- fs, isTFeat feat]

    obIxs = mkOb lbNum
        [ (x, (o, ixMap M.! feat))
        | feat@(OFeature o x) <- oFeats ]
    
    sgIxs = mkSg lbNum
        [ (x, ixMap M.! feat)
        | feat@(SFeature x) <- sFeats ]

    trIxs = mkTr lbNum
        [ ((x, y), ixMap M.! feat)
        | feat@(TFeature x y) <- tFeats ]

    -- | Adjacency vectors.
    mkOb n xs =
        init V.// update
      where
        init = V.replicate n IM.empty
        update = map mkVect $ groupBy ((==) `on` fst) $ sort xs
        mkVect (x:xs) = (fst x, IM.fromList $ sort $ map snd (x:xs))

    mkSg n xs = U.replicate n noKey U.// xs

    mkTr n xs =
        init A.// xs
      where
        init  = A.array bounds [(i, noKey) | i <- A.range bounds]
        bounds = ((0, 0), (n-1, n-1))

    values =
        U.replicate (length fs) 0.0
      U.//
        [(ixMap M.! feat, val) | (feat, val) <- fs]

    checkSet set cont =
        if set == [0 .. length set - 1]
            then cont
            else error "Model.fromList: basic assumption not fulfilled"

mkModel :: [Feature] -> Model
mkModel fs =
    let fSet = S.fromList fs
        fs'  = S.toList fSet
        vs   = replicate (S.size fSet) 0.0
    in  fromList (zip fs' vs)

lbSet :: Model -> [Lb]
lbSet crf = [0 .. lbNum crf - 1]

featToIx :: Feature -> Model -> Maybe Int
featToIx feat crf = case feat of
    SFeature x   -> sFeatToIx x   crf
    TFeature x y -> tFeatToIx x y crf
    OFeature o x -> oFeatToIx o x crf

{-# INLINE oFeatToIx #-}
oFeatToIx :: Ob -> Lb -> Model -> Maybe Int
oFeatToIx o x crf =
    let obMap = obIxs crf V.! x
    in  IM.lookup o obMap

{-# INLINE sFeatToIx #-}
sFeatToIx :: Int -> Model -> Maybe Int
sFeatToIx x crf
    | v == noKey = Nothing
    | otherwise  = Just v
  where
    v = sgIxs crf U.! x

{-# INLINE tFeatToIx #-}
tFeatToIx :: Int -> Int -> Model -> Maybe Int
tFeatToIx x y crf
    | v == noKey = Nothing
    | otherwise  = Just v
  where
    v = trIxs crf A.! (x, y)

onOFeat :: Model -> Ob -> Int -> Double
onOFeat crf o x =
    case oFeatToIx o x crf of
        Just ix -> values crf U.! ix
        Nothing -> 0.0

onSFeat :: Model -> Int -> Double
onSFeat crf x =
    case sFeatToIx x crf of
        Just ix -> values crf U.! ix
        Nothing -> 0.0

onTFeat :: Model -> Int -> Int -> Double
onTFeat crf x y =
    case tFeatToIx x y crf of
        Just ix -> values crf U.! ix
        Nothing -> 0.0
