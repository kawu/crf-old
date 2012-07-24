module Data.CRF.Model.Conns
( Conns (..)
, intersect
) where

import qualified Data.Map as M

import Data.CRF.Base

type FeatMap = M.Map Lb FeatIx

data Conns = Conns
    -- | Set of all labels with dummy FeatIx values.
    { lbIxs :: FeatMap
    -- | Set of acceptable labels on first position in a sentence.
    , sgIxs :: FeatMap
    -- | Set of acceptable labels when known value of the observation.
    , obIxs :: Ob -> FeatMap
    -- | Set of "previous" labels when known value of the current label.
    , pvIxs :: Lb -> FeatMap
    -- | Set of "next" labels when known value of the current label.
    , nxIxs :: Lb -> FeatMap }

-- | Obvious implementation: for every element in the list get FeatIx
-- from a FeatMap.
-- Better one: search keys all at once.  It will require a custom Map or 
-- maybe just using split(Lookup) Data.Map function. List of labels should
-- be changed to a (custom) binary tree in the first place.
--
-- TODO: Handle the case, when the first list is empty (Nothing).
-- TODO: Function has to be quick when the first argument is small!
--
intersect :: [Lb] -> FeatMap -> [(Lb, FeatIx)]
intersect = undefined
