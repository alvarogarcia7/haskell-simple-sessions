import Trees

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
    describe "unfolds a tree" $ do
        it "for the empty root" $ do
            ((unfoldTree [] [id] 1) :: [[Int]]) `shouldBe` ([[],[],[]] :: [[Int]])

