module Trees where

data Tree a = Empty 
             | Root a [Tree a] deriving (Show, Eq)

depth :: Tree a -> Int
depth Empty = 0
depth (Root root []) = 1
depth (Root root children) = 1 + foldl1 max (map depth children)
    
buildTree :: a -> [Tree a] -> Tree a
buildTree root children = Root root children


unfoldTree :: a -> [a -> a] -> Int -> Tree a
unfoldTree root [] depth = Root root []
unfoldTree root fns 1 = Root root (map (\fn -> (Root (fn root) [])) fns)
