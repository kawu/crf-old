{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances #-}

module Data.CRF.Gradient where

import           SGD
import qualified Data.MarkedArray as MA

import           Data.CRF.Types (SentRM)
import           Data.CRF.LogMath (logAdd)
import           Data.CRF.Feature (featuresIn)
import           Data.CRF.Model (Model, expectedFeaturesIn)
import           Data.CRF.Model.Internal (featToIx)
import qualified Data.CRF.Model as CRF

instance SentRM s => DataElem Model s where  

    computeGrad params part buffer =
        let ns = concat $ map featuresIn part
            ens = concat $ map (expectedFeaturesIn params) part
            followPtrs = map $ \(feat, val) -> (featToIx feat params, val)
        in do
            gradient <- MA.consumeWith logAdd (followPtrs ens) buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = CRF.accuracy 
