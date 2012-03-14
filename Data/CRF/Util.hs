module Data.CRF.Util
( partition
) where

import Data.List (transpose)

partition :: Int -> [a] -> [[a]]
partition k = transpose . group k
    where group k [] = []
          group k xs = take k xs
                     : (group k $ drop k xs)

--partition :: Int -> [a] -> [[a]]
--partition k xs = partition' [[] | i <- [1..k]] k xs
--partition' acc k [] = acc
--partition' acc k xs =
--    let (some, rest) = splitAt k xs
--        some' = map (:[]) some ++ repeat []
--        acc' = [y ++ ys | (y, ys) <- zip some' acc]
--    in partition' acc' k rest
