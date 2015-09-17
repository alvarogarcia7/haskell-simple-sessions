main =putStrLn "hello"

st1 = [0,2,4,6,8]
st2 = [1,3,5,7,9]

ste = [0,1,2,3,4,5,6,7,8,9]

merg :: Ord a => [a] -> [a] -> [a]
merg [] [] = []
merg [] (x:xs) = x : merg [] xs
merg (x:xs) [] = x : merg xs []
merg (x:xs) (x':xs') = case x `compare` x' of
    GT -> x': (x  : (merg xs xs'))
    _  -> x : (x' : (merg xs xs'))
