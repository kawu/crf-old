{-# LANGUAGE BangPatterns #-}

module Data.CRF.Codec
( Codec (..)
, fromWords
, encodeO
, encodeL
, decodeL
) where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Binary (Binary, put, get)
import           Data.List (foldl')
import           Data.Maybe (catMaybes, fromJust)

import Data.CRF.Word

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = return . T.decodeUtf8 =<< get

data Codec a = Codec
    { obMap     :: M.Map a Int      -- observations map
    , lbMap     :: M.Map a Int      -- labels map
    , lbMapR    :: M.Map Int a      -- reversed labels map
    , lbDefault :: a }              -- default label
    deriving (Show)

instance (Ord a, Binary a) => Binary (Codec a) where
    put codec = do
        put $ obMap codec
        put $ lbMap codec
        put $ lbMapR codec
        put $ lbDefault codec
    get = do
        obMap <- get
        lbMap <- get
        lbMapR <- get
        lbDefault <- get
        return $ Codec obMap lbMap lbMapR lbDefault

new :: Ord a => a -> Codec a
new lbDef = updateL (Codec M.empty M.empty M.empty lbDef) lbDef

updateMap :: Ord a => M.Map a Int -> a -> M.Map a Int
updateMap mp x =
  case M.lookup x mp of
    Just k -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp

updateO :: Ord a => Codec a -> a -> Codec a
updateO codec x =
    let obMap' = updateMap (obMap codec) x
    in  obMap' `seq` codec { obMap = obMap' }

updateL :: Ord a => Codec a -> a -> Codec a
updateL codec x =
    let lbMap' = updateMap (lbMap codec) x
        lbMapR' = M.insert (lbMap' M.! x) x (lbMapR codec)
    in  lbMap' `seq` lbMapR' `seq`
        codec { lbMap = lbMap', lbMapR = lbMapR' }

update :: Ord a => Codec a -> Word a -> Codec a
update codec0 word =
    codec3
  where
    codec1 = foldl' updateO codec0 (obs word)
    codec2 = foldl' updateL codec1 (lbs word)
    codec3 = foldl' updateL codec2 (map fst $ choice word)

encodeO :: Ord a => Codec a -> a -> Maybe Int
encodeO codec x = x `M.lookup` obMap codec

encodeL :: Ord a => Codec a -> a -> Maybe Int
encodeL codec x = x `M.lookup` lbMap codec

decodeL :: Codec a -> Int -> a
decodeL codec x = lbMapR codec M.! x

fromWords :: Ord a => a -> [Word a] -> Codec a
fromWords lbDef ws = foldl' update (new lbDef) ws
