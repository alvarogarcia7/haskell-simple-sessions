module Permutations where

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "generates permutations" $ do
        it "for the empty list" $ do
            (permutations [] :: [[Int]]) `shouldBe` ([[]] :: [[Int]])

        it "for a one-element list" $ do
            (permutations [1] :: [[Int]]) `shouldBe` ([[1]] :: [[Int]])

        it "for a multiple-element list" $ do
            (permutations [1,2] :: [[Int]]) `shouldBe` ([[1,2], [2,1]] :: [[Int]]);
        
        it "for a multiple-element list" $ do
            (permutations [1,2,3] :: [[Int]]) `shouldBe` ([[1,2,3],[1,3,2],[3,1,2],[2,1,3],[2,3,1],[3,2,1]]::[[Int]]);
            (length (permutations [1,2,3] :: [[Int]])) `shouldBe` (6::Int);


permutations :: [a] -> [[a]]
permutations xs = permutations' [] xs [[]] where
    permutations' _ [] permutations = permutations
    permutations' used (head:rest) accumulated = permutations' (head:used) rest inAllPositions where
            inAllPositions = flatMap (\perm -> map (\pos -> setAt perm pos head) (positionsOf perm)) accumulated where
                positionsOf array = (reverse [0..(length array)])

flatMap f xs = foldl (\ acc ele -> acc++f ele) [] xs

setAt :: [a] -> Int -> a -> [a]
setAt [] 0 ele = [ele]
setAt [] _ _ = []
setAt coll 0 ele = ele:coll
setAt (x:xs) n ele = x:(setAt xs (n-1) ele)

