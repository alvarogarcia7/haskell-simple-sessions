module Trees where


unfoldTree :: a -> [a -> a] -> Int -> [[a]]
unfoldTree root [] depth = []
unfoldTree root fns 1 = [root]:(map (\i -> [i]) (map (\fn -> fn root) fns))
