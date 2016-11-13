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

--        it "with two levels, one function only" $ do
--            ((unfoldTree 0 [id] 2) :: [[Int]]) `shouldBe` ([[0],[[0],[0]]] :: [[Int]])

    describe "the Tree structure" $ do
        it "can be empty" $ do
            depth Empty `shouldBe` 0

        it "can be balanced and having depth 1" $ do
            depth ((Root 1) [Empty]) `shouldBe` 1

        it "can be unbalanced, being deeper on the right" $ do
            depth ((Root 1) [Empty, (Root 2 [Empty])]) `shouldBe` 2

        it "can be unbalanced, being deeper on the left" $ do
            depth ((Root 1) [(Root 2 [Empty]), Empty]) `shouldBe` 2

