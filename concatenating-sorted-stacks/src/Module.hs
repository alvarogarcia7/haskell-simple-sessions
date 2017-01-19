module Module where

merg :: Ord a => [a] -> [a] -> [a]
merg [] [] = []
merg [] (x:xs) = x : merg [] xs
merg y [] = merg [] y
merg (x:xs) (x':xs') = case x `compare` x' of
    GT -> x': (x  : (merg xs xs'))
    _  -> x : (x' : (merg xs xs'))
