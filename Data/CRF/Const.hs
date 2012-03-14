module Data.CRF.Const
( unknown
, dummy
) where

import Data.CRF.Types

-- TODO: zmiana ponizszych funkcji dla zwiekszenia bezpieczenstwa ?
-- unknown: wartosc (obserwacja, pod-tag) nieznana
unknown :: Label
unknown = -1
-- dummy: numer etykiety przed pierwszym slowem zdania
dummy :: Label
dummy = 0

