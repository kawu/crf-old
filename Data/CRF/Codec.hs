module Data.CRF.Codec
( Codec (..)
, fromWords
, encodeO
, encodeL
, decodeL
, encode
, encodeSent
) where

import qualified Data.Map as M
import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Binary (Binary, put, get)
import Data.List (foldl')

import qualified Data.CRF.Const as Const
import Data.CRF.InOut

instance Binary T.Text where
    put = put . T.encodeUtf8
    get = return . T.decodeUtf8 =<< get

data Codec a = Codec
    { oMap :: M.Map a Int       -- observations map
    , lMap :: M.Map a Int       -- labels map
    , lRevMap :: M.Map Int a }  -- reversed labels map
    deriving (Show)

instance (Ord a, Binary a) => Binary (Codec a) where
    put codec = do
        put $ oMap codec
        put $ lMap codec
        put $ lRevMap codec
    get = do
        oMap <- get
        lMap <- get
        lRevMap <- get
        return Codec { oMap = oMap, lMap = lMap, lRevMap = lRevMap }

empty :: Codec a
empty = Codec { oMap = M.empty, lMap = M.empty, lRevMap = M.empty }

updateMap :: (Ord a) => M.Map a Int -> a -> M.Map a Int
updateMap mp x =
  case M.lookup x mp of
    Just k -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp + 1

updateO :: Ord a => Codec a -> a -> Codec a
updateO codec x =
    let oMap' = updateMap (oMap codec) x
    in  oMap' `seq`
        Codec {oMap=oMap', lMap=lMap codec, lRevMap=lRevMap codec}

updateL :: Ord a => Codec a -> a -> Codec a
updateL codec x =
    let lMap' = updateMap (lMap codec) x
        lRevMap' = M.insert (lMap' M.! x) x (lRevMap codec)
    in  lMap' `seq` lRevMap' `seq`
        Codec {oMap=oMap codec, lMap=lMap', lRevMap=lRevMap'}

update :: Ord a => Codec a -> WordRM a -> Codec a
update codec0 (WordRM obvs rs ys) = codec2
  where
    labels = rs ++ [y | (y, _) <- ys]
    codec1 = foldl' updateO codec0 obvs
    codec2 = foldl' updateL codec1 labels

-- TODO: Powinny byc rozrozniane wartosci unknownLabel i unknownObv ?
encodeWith :: (Ord a) => a -> M.Map a Int -> Int
encodeWith x mp = case M.lookup x mp of
    Just k -> k
    Nothing -> Const.unknown

encodeO :: Ord a => Codec a -> a -> Int
encodeO codec x = x `encodeWith` oMap codec

encodeL :: Ord a => Codec a -> a -> Int
encodeL codec x = x `encodeWith` lMap codec

decodeL :: Codec a -> Int -> a
decodeL codec x = lRevMap codec M.! x

encode :: Ord a => Codec a -> WordRM a -> WordRM Int
encode codec (WordRM obvs rs ys) = WordRM obvs' rs' ys'
  where
    obvs' = map encodeO' obvs
    rs' = map encodeL' rs
    ys' = [ (encodeL' y, prob)
          | (y, prob) <- ys ]
    encodeO' = encodeO codec
    encodeL' = encodeL codec

encodeSent :: Ord a => Codec a -> SentRM a -> SentRM Int
encodeSent codec = map $ encode codec

fromWords :: Ord a => [WordRM a] -> Codec a
fromWords ws = foldl' update empty ws
