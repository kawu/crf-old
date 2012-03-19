{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances #-}

module Data.CRF.Gradient where

import           SGD
import qualified Data.MarkedArray as MA

import           Data.CRF.Types (SentRM)
import           Data.CRF.LogMath (logAdd)
import           Data.CRF.Feature (featuresIn)
import           Data.CRF.Model (Model, expectedFeaturesIn)
import qualified Data.CRF.Model as CRF

instance SentM s => DataElem Model s where  

    computeGrad crf part buffer =
        let ns = concat $ map featuresIn part
            ens = concat $ map (expectedFeaturesIn crf) part
            followPtrs = map $ \(feat, val) -> (featToIx feat crf, val)
        in do
            gradient <- MA.consumeWith logAdd ens buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = CRF.accuracy 
