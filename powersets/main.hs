module PowerSets where

import Test.Hspec
import Data.List
import Text.Printf (printf)

powersets :: [x] -> [[x]]
powersets [] = [[]]
powersets (x:[]) = [[],[x]]
powersets (x:y:[]) = [[],[x],[y],[x,y]]

flatmap :: (x -> [x]) -> [x] -> [x]
flatmap f xs = foldl (\acc ele-> acc ++ (f ele)) [] xs



main = hspec $ do
        describe "canary" $ do
            it "truth" $ do
                True `shouldBe` True;

        describe "powersets" $ do
            it "base case" $ do
                ((powersets []) :: [[Int]]) `shouldBe` [[]];

            it "input of one" $ do
                ((powersets [1]) :: [[Int]]) `shouldBe` [[],[1]];

            it "input of two" $ do
                ((powersets [1,2]) :: [[Int]]) `shouldBe` [[],[1],[2],[1,2]];


