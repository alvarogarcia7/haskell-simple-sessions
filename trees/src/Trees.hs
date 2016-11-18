module Trees where


unfoldTree :: a -> [a -> a] -> Int -> [[a]]
unfoldTree root [] depth = []
unfoldTree root fns 1 = [root]:(map (\i -> [i]) (map (\fn -> fn root) fns))

data Tree a = Empty 
             | Root a [Tree a] deriving (Show, Eq)

depth :: Tree a -> Int
depth Empty = 0
depth (Root root children) = 1 + foldl1 max (map depth children)
    

