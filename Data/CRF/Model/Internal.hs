module Data.CRF.Model.Internal
( Model
-- , fromList
-- , toList
, makeModel
, onOFeat
, onTFeat
, featToIx
) where

import Control.Monad (forM_)
import Data.Maybe (fromJust)
import Data.Ix (range, inRange, rangeSize)

import qualified Data.Set as Set
import qualified Data.Array as A
import qualified Data.Array.IO as A
import qualified Data.Array.MArray as A
import qualified Data.Array.Unboxed as UA
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed as U

import Data.Binary

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)

import SGD

import Data.CRF.Types
import Data.CRF.Feature
import Data.CRF.LogMath
import Data.CRF.Vector.Binary

data Model = Model
    { obvsIxs   :: A.Array Int (IM.IntMap Int)
    , transIxs  :: UA.UArray (Label, Label) Int
    , values    :: U.Vector Double }

instance ParamCore Model where

    unsafeConsume f xs crf = do
        values' <- unsafeConsume f xs $ values crf
        return $ Model (obvsIxs crf) (transIxs crf) values' 

    unsafeMap f crf = do
        values' <- unsafeMap f $ values crf
        return $ Model (obvsIxs crf) (transIxs crf) values' 

    size = size . values

instance Binary Model where
    put crf = do
        put $ obvsIxs crf
        put $ transIxs crf
        put $ values crf
    get = do
        obvsIxs <- get
        transIxs <- get
        values <- get
        return Model { obvsIxs = obvsIxs
                     , transIxs = transIxs
                     , values = values }

instance Show Model where
    show = unlines . map show . toList

toList :: Model -> [(Feature, Double)]
toList crf =
    [ (TFeature x y, v)
    | ((x, y), v) <- map dcd $ UA.assocs $ transIxs crf ]
    ++
    [ (OFeature o x, v)
    | (o, om) <- A.assocs $ obvsIxs crf
    , (x, v) <- map dcd $ IM.toList om ]
  where
    dcd (key, ix) = (key, values crf U.! ix)

-- | Construct CRF model from associations list.  There should be
--   no repetition of features in the input list.
fromList :: [(Feature, Double)] -> Model
fromList fs = {-# SCC "modelFromList" #-}
    let featSublabels (OFeature _ x) = [x]
        featSublabels (TFeature x y) = [x, y]
        featObvs (OFeature o _) = [o]
        featObvs _ = []
    
        labels = Set.toList $ Set.fromList
               $ concat $ map featSublabels $ map fst fs
        lmax = maximum labels
        omax = maximum $ concat $ map featObvs $ map fst fs

        trsFeats = [feat | (feat, val) <- fs, isTransFeat feat]
        obsFeats = [feat | (feat, val) <- fs, isObserFeat feat]

        -- TODO: Brać pod uwagę *wprost* etykiety unknown, dummy.
        transIxs = UA.array ((-1, -1), (lmax, lmax))
            [ ((x, y), ix)
            | (TFeature x y, ix) <- zip trsFeats [0..] ]

        obvsIxs = A.array (1, omax)
            [ (o, IM.fromList [])
            | o <- [1 .. omax] ]
          A.//
            [ (o, IM.fromList
                [ (x, ix)
                | (OFeature _ x, ix) <- fs ])
            | (o, fs) <- groupObsFeatures obsFeats' ]
          where
            obsFeats' = zip obsFeats [length trsFeats ..]

        groupObsFeatures fs = IM.toList $
            IM.fromListWith (++) 
                [ (o, [(feat, ix)])
                | (feat@(OFeature o _), ix) <- fs ]

        values =
            U.replicate (length fs) 0.0
          U.//
            [(featToIx feat model, val) | (feat, val) <- fs]

        model = Model
            { transIxs = transIxs
            , obvsIxs = obvsIxs
            , values = values }
    in model

makeModel :: [Feature] -> Model
makeModel fs =
    let fSet = Set.fromList fs
        fs'  = Set.toList fSet
        vs   = replicate (Set.size fSet) 0.0
    in  fromList (zip fs' vs)

oFeatToIx :: Obser -> Label -> Model -> Maybe Int
oFeatToIx o x crf =
    let obsMap = obvsIxs crf A.! o
    in  IM.lookup x obsMap

-- TODO: dodac wersja 'unsafe' -- zmiana może być kluczowa
-- dla szybkości działania.
tFeatToIx :: Label -> Label -> Model -> Maybe Int
tFeatToIx x y crf =
    case inRange (UA.bounds $ transIxs crf) (x, y) of
        True    -> Just $ transIxs crf UA.! (x, y)
        False   -> Nothing

featToIx :: Feature -> Model -> Int
featToIx feat crf = fromJust $ case feat of
    TFeature x y -> tFeatToIx x y crf
    OFeature o x -> oFeatToIx o x crf

onOFeat :: Obser -> Label -> Model -> Double
onOFeat o x crf =
    case oFeatToIx o x crf of
        Just ix -> values crf U.! ix
        Nothing -> 0.0

onTFeat :: Label -> Label -> Model -> Double
onTFeat x y crf =
    case tFeatToIx x y crf of
        Just ix -> values crf U.! ix
        Nothing -> 0.0
