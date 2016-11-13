import Trees

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "unfolds a tree" $ do
        it "for the empty array of unfolding functions" $ do
            ((unfoldTree 0 [] 1) :: [[Int]]) `shouldBe` ([] :: [[Int]])

        -- this returns a list of elements (size 2)
        it "with one level only, with one function only" $ do
            ((unfoldTree 0 [id] 1) :: [[Int]]) `shouldBe` ([[0],[0]] :: [[Int]])

        it "with one level only, with two functions only" $ do
            ((unfoldTree 0 [id, id] 1) :: [[Int]]) `shouldBe` ([[0],[0],[0]] :: [[Int]])

        it "with two levels, one function only" $ do
            ((unfoldTree 0 [id] 2) :: [[Int]]) `shouldBe` ([[0],[[0],[0]]] :: [[Int]])

