module Permutations where

import Data.List

permutate :: [a] -> [[a]]
permutate [] = []
permutate x = permutate' x [[]] where
  permutate' elements accumulated = case length elements of
    1 -> [elements]
    2 -> [elements, reverse elements]
    n -> foldl (++) [] $ map (\comb -> addInAllPositions (head x) comb ) $ permutate' (tail x) accumulated


addInAllPositions element array = map (\position -> setAt array position element) (reverse [0..(length array)])

-- TODO AGB Replace function with Data.List.Tools::setAt
setAt :: [a] -> Int -> a -> [a]
setAt [] 0 ele = [ele]
setAt [] _ _ = []
setAt coll 0 ele = ele:coll
setAt (x:xs) n ele = x:(setAt xs (n-1) ele)

