module IntroToTypes where

import Text.Printf (printf)
import Prelude hiding (flip)

type Serie a = [a]
type Cart a = [Serie a]

serie :: b -> Serie b
serie b = [b]

serieFor :: [b] -> Serie b
serieFor = id

cart :: Serie s -> Cart s
cart s = [s]

length' :: Cart c -> Int
length' [serie] = length'' serie
length' (serie:restOfCart) = length'' serie + length' restOfCart

length'' :: Serie s -> Int
length'' [] = 0
length'' (book:restOfSerie) = 1 + length'' restOfSerie

flip :: Cart c -> Cart c
flip = id

