import TicTacToe

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
       True `shouldBe` True

  describe "tic tac toe" $ do
    it "should be an empty board to start with" $ do
       board game `shouldBe` [[Nothing,Nothing,Nothing], [Nothing,Nothing,Nothing], [Nothing,Nothing,Nothing]]

    it "should have the first player as 'X'" $ do
       currentPlayer game `shouldBe` 'X' 

    describe "first move only" $ do
      it "should store the first movement" $ do
        board (makeAMove game (0,0)) `shouldBe` [[Just 'X', Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

