module Data.CRF.Model.Internal
( Model
-- , fromList
-- , toList
, makeModel
, onOFeat
, onTFeat
, featToIx
) where

-- import Control.Monad (forM_)
-- import Data.Maybe (fromJust)
-- import Data.Ix (range, inRange, rangeSize)
-- 
-- import qualified Data.Set as Set
-- import qualified Data.Array as A
-- import qualified Data.Array.IO as A
-- import qualified Data.Array.MArray as A
-- import qualified Data.Array.Unboxed as UA
-- import qualified Data.IntMap as IM
-- import qualified Data.Vector.Unboxed as U
-- 
-- import Data.Binary
-- 
-- import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, forkIO)
-- 
-- import SGD
-- 
-- import Data.CRF.Types
-- import Data.CRF.Feature
-- import Data.CRF.LogMath
-- import Data.CRF.Vector.Binary

import           Data.CRF.Base

type ValIx = Int
type LbIx  = (Lb, ValIx)

data Model = Model
    -- | Set of all labels present in data set == [0 .. labelNum - 1].
    { labelNum  :: Int
--     , obIxs     :: A.Array Ob (IM.IntMap ValIx)
    , obIxs     :: V.Vector (U.Vector LbIx)
    -- | Set of "previous" indices when known value of the current label.
    , prevIxs   :: V.Vector (U.Vector LbIx)
    -- | Set of "next" indices when known value of the current label.
    , nextIxs   :: V.Vector (U.Vector LbIx)
    , values    :: U.Vector Double }

instance ParamCore Model where

    unsafeConsume f xs crf = do
        values' <- unsafeConsume f xs $ values crf
        return $ crf { values = values' }

    unsafeMap f crf = do
        values' <- unsafeMap f $ values crf
        return $ crf { values = values' }

    size = size . values

-- -- | Acceptable "previous" labels when known value of the current label.
-- prevInterpIxs :: Sent s => Model -> Label -> s -> Int -> [Int]
-- prevInterpIxs crf a sent k = prevIxs crf V.! a
-- --   where
-- --     xs  = interpsOn sent k
-- --     xs' = prevLs crf V.! a
-- 
-- -- | Acceptable "next" labels when known value of the current label.
-- -- FIXME: use the interpsOn information!
-- nextInterpIxs :: SentR s => Model -> Label -> s -> Int -> [Int]
-- nextInterpIxs crf a sent k = nextIxs crf V.! a
-- 
-- pairInterpIxs :: SentR s => Model -> s -> Int -> [(Int, Int)]
-- pairInterpIxs crf sent k =
--     [ (i, j)
--     | (a, i) <- zip (interpsOn sent k) [0..]
--     , j <- prevInterpIxs crf a sent k ]

instance Binary Model where
    put crf = do
        put $ labelNum crf
        put $ obIxs crf
        put $ prevIxs crf
        put $ nextIxs crf
        put $ values crf
    get = do
        labelNum <- get
        obIxs <- get
        prevIxs <- get
        nextIxs <- get
        values <- get
        return $ Model labelNum obIxs prevIxs nextIxs values

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
    let featLabels (OFeature _ x) = [x]
        featLabels (TFeature x y) = [x, y]
        featObs (OFeature o _) = [o]
        featObs _ = []
    
        labels = Set.toList $ Set.fromList
               $ concat $ map featLabels $ map fst fs
        lmax = maximum labels
        omax = maximum $ concat $ map featObs $ map fst fs

        trsFeats = [feat | (feat, val) <- fs, isTFeat feat]
        obsFeats = [feat | (feat, val) <- fs, isOFeat feat]

        -- TODO: Brać pod uwagę *wprost* etykiety unknown, dummy.
        -- ==================================================================================================================================================================================================================================================================== TUTAJ SKONCZYLES !
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
