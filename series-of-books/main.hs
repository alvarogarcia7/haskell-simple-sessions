

merge :: [[b]] -> b -> [[[b]]]
merge cart book = [[book]:cart, map (\serie -> book:serie) cart, [[book]], cart]

-- generates carts of books, which the following restrictions
--  * order of books in a series does not matter
--  * order of series does not matter
--  * the amount of books in a cart cannot of be changed
-- TODO: DEFECT: it generates duplicates
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

priceFor :: [[b]] -> Double
priceFor cart = foldl (+) 0.0 (map priceForSerie cart)

priceForSerie :: [b] -> Double
priceForSerie serie = serieLength * 8.0 * (1.0 - discount) where
    discount = (fromIntegral ((length serie) - 1)::Double) * 0.05
    serieLength = fromIntegral ((length serie))::Double

--
--Main> map (\cart -> (cart, priceFor cart)) (cartsFor [1,2])
--[([[2],[1],[]],16.0),([[2],[1]],16.0),([[2,1]],15.2),([[2],[1]],16.0),([[2,1]],15.2)]

