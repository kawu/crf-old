{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Data.CRF.RCRF.Gradient where

import           Control.Applicative ((<$>))
import           Data.Maybe (catMaybes)
import qualified Data.MarkedArray as MA

import           SGD

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
        let ns  = catMaybes $ map followPtr
                $ concatMap (uncurry featuresIn) part
            ens = catMaybes $ map followPtr
                $ concatMap (expectedFeaturesIn crf . fst) part
            followPtr (feat, val) = (,val) <$> featToIx feat crf
        in do
            gradient <- MA.consumeWith logAdd ens buffer
                    >>= MA.mapArray (\v -> - exp v) 
                    >>= MA.consumeWith (+) ns
            return gradient

    accuracy = CRF.accuracy 
