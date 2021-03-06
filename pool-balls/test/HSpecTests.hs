
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

  describe "Problem 1, 2: minimum number of swaps" $ do
    it "Only one way of swapping" $ do
      reorder "ABC" "BAC" `shouldBe` [[0,1]]

    it "Multiple ways of swapping" $ do
      reorder "ABA" "BAA" `shouldBe` [[0,1]]

  describe "Problem 3: maximum number of swaps" $ do
    it "Only one way of swapping" $ do
      reorderMax "AAB" "BAA" `shouldBe` [[1,2], [0,1]]
      length (reorderMax "AAAB" "BAAA") `shouldBe` 3

