module Permutations where

permutations :: [a] -> [[a]]
permutations xs = permutations' [] xs [[]] where
    permutations' _ [] permutations = permutations
    permutations' used (head:rest) accumulated = permutations' (head:used) rest inAllPositions where
        inAllPositions = flatMap (\perm -> map (setAt' perm) (positionsOf perm)) accumulated where
            setAt' perm position = setAt perm position head
            positionsOf array = (reverse [0..(length array)])

flatMap f xs = foldl (++) [] $ map f xs

-- TODO AGB Replace function with Data.List.Tools::setAt
setAt :: [a] -> Int -> a -> [a]
setAt [] 0 ele = [ele]
setAt [] _ _ = []
setAt coll 0 ele = ele:coll
setAt (x:xs) n ele = x:(setAt xs (n-1) ele)

