{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.CRF.RCRF.Gradient where

import           SGD
import qualified Data.MarkedArray as MA

import           Data.CRF.Base
import           Data.CRF.R
import           Data.CRF.Y

import           Data.CRF.LogMath (logAdd)
import           Data.CRF.Feature (featuresIn)
import           Data.CRF.RCRF.Model (Model, featToIx)
import           Data.CRF.RCRF.Infere (expectedFeaturesIn)
import qualified Data.CRF.RCRF.Infere as CRF

instance DataElem Model (Sent R, Sent Y) where  

    computeGrad crf part buffer =
        let ns = concatMap (uncurry featuresIn) part
            ens = concatMap (expectedFeaturesIn crf . fst) part
            followPtrs = map $ \(feat, val) -> (featToIx feat crf, val)
        in do
            gradient <- MA.consumeWith logAdd (followPtrs ens) buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = CRF.accuracy 
