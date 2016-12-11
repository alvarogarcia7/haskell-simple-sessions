import PoolBalls

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
      True `shouldBe` True

  describe "apply a swap" $ do
    it "swaps the balls" $ do
      apply [0,1] "012" `shouldBe` "102"

    it "problem 1 - minimum number of swaps" $ do
      reorder "ABC" "BAC" `shouldBe` [[0,1]]

    it "problem 1 - minimum number of swaps, given multiple paths" $ do
      reorder "ABA" "BAA" `shouldBe` [[0,1]]
