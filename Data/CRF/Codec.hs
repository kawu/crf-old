{-# LANGUAGE BangPatterns #-}

module Data.CRF.Codec
( Codec (..)
, mkCodec
, encodeO
, encodeL
, encodeL'
, decodeL
) where

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Binary (Binary, put, get)
import           Data.List (foldl')
import           Data.Maybe (catMaybes, fromJust)

import Data.CRF.Word

data Codec a b = Codec
    { obMap     :: M.Map a Int      -- observations map
    , lbMap     :: M.Map b Int      -- labels map
    , lbMapR    :: M.Map Int b      -- reversed labels map
    -- | Default label, which represents an unknown label.
    -- It should not be a member of (M.keys lbMap).
    -- TODO: Check this condition on startup.
    , lbDef     :: b }
    deriving (Show)

instance (Ord a, Binary a, Ord b, Binary b) => Binary (Codec a b) where
    put codec = do
        put $ obMap codec
        put $ lbMap codec
        put $ lbMapR codec
        put $ lbDef codec
    get = do
        obMap <- get
        lbMap <- get
        lbMapR <- get
        lbDef <- get
        return $ Codec obMap lbMap lbMapR lbDef

new :: Ord b => b -> Codec a b
new lbDef = updateL (Codec M.empty M.empty M.empty lbDef) lbDef

updateMap :: Ord a => M.Map a Int -> a -> M.Map a Int
updateMap mp x =
  case M.lookup x mp of
    Just k -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp

updateO :: Ord a => Codec a b -> a -> Codec a b
updateO codec x =
    let obMap' = updateMap (obMap codec) x
    in  obMap' `seq` codec { obMap = obMap' }

updateL :: Ord b => Codec a b -> b -> Codec a b
updateL codec x =
    let lbMap' = updateMap (lbMap codec) x
        lbMapR' = M.insert (lbMap' M.! x) x (lbMapR codec)
    in  lbMap' `seq` lbMapR' `seq`
        codec { lbMap = lbMap', lbMapR = lbMapR' }

update :: (Ord a, Ord b) => Codec a b -> Word a b -> Codec a b
update codec0 word =
    codec3
  where
    codec1 = foldl' updateO codec0 (obs word)
    codec2 = foldl' updateL codec1 (lbs word)
    codec3 = foldl' updateL codec2 (map fst $ choice word)

encodeO :: Ord a => Codec a b -> a -> Maybe Int
encodeO codec x = x `M.lookup` obMap codec

encodeL :: Ord b => Codec a b -> b -> Maybe Int
encodeL codec x = x `M.lookup` lbMap codec

-- | Just like encodeL, but returns default label instead of Nothing.
encodeL' :: Ord b => Codec a b -> b -> Int
encodeL' codec x = case x `M.lookup` lbMap codec of
    Just y  -> y
    Nothing -> lbMap codec M.! lbDef codec

decodeL :: Codec a b -> Int -> b
decodeL codec x = lbMapR codec M.! x

mkCodec :: (Ord a, Ord b) => b -> [Word a b] -> Codec a b
mkCodec lbDef = foldl' update (new lbDef)
