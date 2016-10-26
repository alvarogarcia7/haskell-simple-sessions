module BookGeneration where

import Test.Hspec
import Text.Printf (printf)

import Data.List
import Debug.Trace

type Serie a = [a]
type Cart a = [Serie a]

merge :: Show b => [[b]] -> b -> [[[b]]]
--merge cart book = trace ("" ++ show cart ++ " " ++ show book ++ " = " ++ show x) x where
merge cart book = x where
    x = ([book]:cart) : map (\serieIndex -> addBookAt book cart serieIndex) [0..(length cart)] where
        addBookAt :: b -> [[b]] -> Int -> [[b]]
        addBookAt book [] 0 = [[book]]
        addBookAt book (serie:restOfCart) 0 = (book:serie):restOfCart
        addBookAt book (serie:restOfCart) n = serie:(addBookAt book restOfCart $ n - 1)
        addBookAt _ _ _ = error("aaaa")

-- generates carts of books, which the following restrictions
--  * order of books in a series does not matter
--  * order of series does not matter
--  * the amount of books in a cart cannot of be changed
cartsFor :: Show b => Eq b => [b] -> [[[b]]]
cartsFor books = nub (sortBy sortCarts (filter (\cart -> (length' cart == (length books))) (removeEmptySeries (cartsFor' [[[]]] books))))

removeEmptySeries :: [[[b]]] -> [[[b]]]
removeEmptySeries carts = map removeEmptySeries' carts where
    removeEmptySeries' cart = filter (\serie -> length serie > 0) cart

sortCarts a b | length a > length b = GT
              | length a < length b = LT
              | length a == length b = sortSeries a b

sortSeries a b | length a > length b = GT
               | length a < length b = LT
               | otherwise = EQ

cartsFor' accCarts [] = accCarts
cartsFor' accCarts (x:xs) = cartsFor' (flatMap (\cart -> merge cart x) accCarts) xs

onlyValid :: [[[b]]] -> [[[b]]]
onlyValid carts = map onlyValid' carts where
    onlyValid' cart = filter (\x -> True) cart

length' :: [[b]] -> Int
length' [] = 0
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
--[([[2],[1],[]],16.0),([[2],[1]],16.0),([[2,1]],15.2)]


main = hspec $ do
    describe "generates carts" $ do
        it "no books turns into a single cart" $
            (cartsFor [] :: [[[Int]]]) `shouldBe` ([[]] :: [[[Int]]])
