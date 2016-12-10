import PoolBalls

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
      True `shouldBe` True

  describe "apply a swap" $ do
    it "swaps the balls" $ do
      apply [0,1] [0,1,2] `shouldBe` [1,0,2]
