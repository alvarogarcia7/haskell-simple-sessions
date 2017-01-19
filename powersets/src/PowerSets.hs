module PowerSets where

import Data.List
import Text.Printf (printf)

powersets :: [x] -> [[x]]
powersets xs = sortBy size $ powersets' xs where
    powersets' xs = flatmap addHeadTo (pieces xs) where
        addHeadTo [] = [[]]
        addHeadTo (x':xs') = map (x':) (powersets' xs')

-- Pieces: generates immediate results of removing elements from the left, one by one
-- See tests
pieces :: [x] -> [[x]]
pieces = tails

size a b | length a > length b = GT
         | length a < length b = LT
         | length a == length b = EQ


flatmap :: (x -> [x]) -> [x] -> [x]
flatmap f xs = foldl (\acc ele-> acc ++ (f ele)) [] xs

