import Trees

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "unfolds a tree" $ do
        it "for the empty root" $ do
            ((unfoldTree 0 [id] 1) :: [[Int]]) `shouldBe` ([[],[],[]] :: [[Int]])

        it "for the empty array of unfolding functions" $ do
            ((unfoldTree 0 [] 1) :: [[Int]]) `shouldBe` ([] :: [[Int]])

