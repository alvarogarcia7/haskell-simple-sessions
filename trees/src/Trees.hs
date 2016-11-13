module Trees where


unfoldTree :: a -> [a -> a] -> Int -> [[b]]
unfoldTree root fns depth = [[],[],[]]
