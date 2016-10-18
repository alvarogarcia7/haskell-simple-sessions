

merge :: [[b]] -> b -> [[[b]]]
merge cart book = [[book]:cart, map (\serie -> book:serie) cart, [[book]], cart]

cartsFor :: [b] -> [[[b]]]
cartsFor books = filter (\cart -> (length' cart == (length books))) (cartsFor' [[[]]] books)
    where cartsFor' accCarts [] = accCarts
          cartsFor' accCarts (x:xs) = cartsFor' (flatMap (\cart -> merge cart x) accCarts) xs

length' :: [[b]] -> Int
length' [x] = length'' x
length' (x:y) = length'' x + length' y

length'' :: [b] -> Int
length'' [] = 0
length'' (x:xs) = 1 + length'' xs

flatMap :: (a -> [b]) -> [a] -> [b]
flatMap f xs = foldl (\acc x -> (acc ++ f x)) [] xs

