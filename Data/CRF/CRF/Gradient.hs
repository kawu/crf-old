{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.CRF.CRF.Gradient where

import           SGD
import qualified Data.MarkedArray as MA

import           Data.CRF.Base
import           Data.CRF.X
import           Data.CRF.Y

import           Data.CRF.LogMath (logAdd)
import           Data.CRF.Feature (featuresIn)
import           Data.CRF.CRF.Model (Model, featToIx)
import           Data.CRF.CRF.Infere (expectedFeaturesIn)
import qualified Data.CRF.CRF.Infere as CRF

instance DataElem Model (Sent X, Sent Y) where  

    computeGrad crf part buffer =
        let ns = concatMap (uncurry featuresIn) part
            ens = concatMap (expectedFeaturesIn crf . fst) part
            followPtrs = map $ \(feat, val) -> (featToIx crf feat, val)
        in do
            gradient <- MA.consumeWith logAdd ens buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = CRF.accuracy 
