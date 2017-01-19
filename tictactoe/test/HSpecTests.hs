import TicTacToe

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
       True `shouldBe` True

  describe "tic tac toe" $ do
    it "should be an empty board to start with" $ do
       game `shouldBe` [[Nothing,Nothing,Nothing], [Nothing,Nothing,Nothing], [Nothing,Nothing,Nothing]]

