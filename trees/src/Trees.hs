module Trees where


unfoldTree :: a -> [a -> a] -> Int -> [[a]]
unfoldTree root [] depth = []
unfoldTree root fns 1 = [root]:(map (\i -> [i]) (map (\fn -> fn root) fns))

data Tree a = Empty 
             | Root a (Tree a) (Tree a) deriving (Show, Eq)

depth :: Tree a -> Int
depth Empty = 0
depth (Root root left right) = 1 + depth right
    

