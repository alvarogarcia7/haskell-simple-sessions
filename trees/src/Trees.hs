module Trees where


unfoldTree :: a -> [a -> a] -> Int -> [[b]]
unfoldTree root [] depth = []
unfoldTree root fns depth = [[],[],[]]
