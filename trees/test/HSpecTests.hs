import Trees

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "unfolds a tree" $ do
        it "for the empty array of unfolding functions" $ do
            (unfoldTree 0 [] 1) `shouldBe` (leaf 0 :: Tree Int)

        -- this returns a list of elements (size 2)
        it "with one level only, with one function only" $ do
            (unfoldTree 0 [id] 1) `shouldBe` (Root 0 [leaf 0] :: Tree Int)

        it "with one level only, with two functions only" $ do
            (unfoldTree 0 [id, id] 1) `shouldBe` (Root 0 [leaf 0, leaf 0] :: Tree Int)

        it "with two levels, one function only" $ do
            (unfoldTree 0 [id] 2) `shouldBe` (Root 0 [Root 0 [leaf 0]])

        it "also works with deeper tree" $ do
            depth (unfoldTree 0 [id] 100) `shouldBe` 101 


    describe "Pascal's triangle" $ do
        it "calculates the first level" $ do
            rootOf (pascal 1) `shouldBe` 1

        it "calculates the second level" $ do
            map rootOf (childrenOf (pascal 2)) `shouldBe` [1,2,1]

    describe "the Tree structure" $ do
        describe "has a depth" $ do
            it "can be empty" $ do
                depth Empty `shouldBe` 0

            it "can be balanced and having depth 1" $ do
                depth ((Root 1) [Empty]) `shouldBe` 1

            it "can be balanced and having depth 1 - just another way of representing the same tree" $ do
                depth ((Root 1) []) `shouldBe` 1

            it "can be unbalanced, being deeper on the right" $ do
                depth ((Root 1) [Empty, (Root 2 [])]) `shouldBe` 2

            it "can be unbalanced, being deeper on the left" $ do
                depth ((Root 1) [(Root 2 []) ]) `shouldBe` 2

        describe "build a tree" $ do
            it "appends a root and its children" $ do
                let leftTree = Root "left" []
                let rightTree = Root "right" []
                (buildTree "root" [leftTree, rightTree]) `shouldBe` (Root "root" [leftTree, rightTree])

