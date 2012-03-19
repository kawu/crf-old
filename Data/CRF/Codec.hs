{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
           , FunctionalDependencies #-}

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
import qualified Data.Text.Encoding as T
import           Data.Binary (Binary, put, get)
import           Data.List (foldl')
import           Data.ListLike (ListLike, fromList, toList)

import qualified Data.CRF.Const as Const
import           Data.CRF.Base (X, Y, Xs, XYs)


class HasObs w t | w -> t where
    obs :: w -> [t]

class HasChoice w t | w -> t where
    choice :: w -> [(t, Double)]

class (HasChoice w t, HasObs w t) => IsWord w t where
instance (HasChoice w t, HasObs w t) => IsWord w t where


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
        return $ Codec oMap lMap lRevMap

empty :: Codec a
empty = Codec M.empty M.empty M.empty

updateMap :: Ord a => Int -> M.Map a Int -> a -> M.Map a Int
updateMap shift mp x =
  case M.lookup x mp of
    Just k -> mp
    Nothing -> M.insert x n mp
  where
    !n = M.size mp + shift

updateO :: Ord a => Codec a -> a -> Codec a
updateO codec x =
    let oMap' = updateMap 0 (oMap codec) x
    in  oMap' `seq` codec { oMap = oMap' }

updateL :: Ord a => Codec a -> a -> Codec a
updateL codec x =
    let lMap' = updateMap 1 (lMap codec) x
        lRevMap' = M.insert (lMap' M.! x) x (lRevMap codec)
    in  lMap' `seq` lRevMap' `seq`
        Codec (oMap codec) lMap' lRevMap'

update :: (IsWord w a, Ord a) => Codec a -> w -> Codec a
update codec0 word =
    codec2
  where
    codec1 = foldl' updateO codec0 (obs word)
    codec2 = foldl' updateL codec1 [y | (y, _) <- choice word]

encodeWith :: Ord a => a -> M.Map a Int -> Int
encodeWith x mp = case M.lookup x mp of
    Just k -> k
    Nothing -> Const.unknown

encodeO :: Ord a => Codec a -> a -> Int
encodeO codec x = x `encodeWith` oMap codec

encodeL :: Ord a => Codec a -> a -> Int
encodeL codec x = x `encodeWith` lMap codec

decodeL :: Codec a -> Int -> a
decodeL codec x = lRevMap codec M.! x

encode :: (HasObs w a, Ord a) => Codec a -> w -> X
encode codec word = fromList $ map (encodeO codec) (obs word)

encodeSent :: (ListLike s w, HasObs w a, Ord a) => Codec a -> s -> Xs
encodeSent codec = fromList . map (encode codec) . toList

encode' :: (IsWord w a, Ord a) => Codec a -> w -> (X, Y)
encode' codec word =
    (fromList obs', fromList choice')
  where
    obs'    = map (encodeO codec) (obs word)
    choice' = [ (encodeL codec y, prob)
              | (y, prob) <- choice word ]

encodeSent' :: (ListLike s w, IsWord w a, Ord a) => Codec a -> s -> XYs
encodeSent' codec = fromList . map (encode' codec) . toList

fromWords :: (IsWord w a, Ord a) => [w] -> Codec a
fromWords ws = foldl' update empty ws
