{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.CRF.RCRF2.Gradient where

import           Data.Maybe (fromJust)
import qualified Data.MarkedArray as MA

import           SGD

import           Data.CRF.Base
import           Data.CRF.R
import           Data.CRF.Y

import           Data.CRF.LogMath (logAdd)
import           Data.CRF.Feature (featuresIn)
import           Data.CRF.RCRF2.Model (Model, featToIx)
import           Data.CRF.RCRF2.Infere (expectedFeaturesIn)
import qualified Data.CRF.RCRF2.Infere as CRF

instance DataElem Model (Sent R, Sent Y) where  

    computeGrad crf part buffer =
        let ns = concatMap (uncurry featuresIn) part
            ens = concatMap (expectedFeaturesIn crf . fst) part
            followPtrs = map followPtr
            followPtr (feat, val) = (fromJust (featToIx crf feat), val)
        in do
            gradient <- MA.consumeWith logAdd ens buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) (followPtrs ns)
            return gradient

    accuracy = CRF.accuracy 
