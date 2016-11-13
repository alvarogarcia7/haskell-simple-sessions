module Trees where


unfoldTree :: a -> [a -> a] -> Int -> [[a]]
unfoldTree root [] depth = []
unfoldTree root _ 1 = [[root],[root],[root]]
