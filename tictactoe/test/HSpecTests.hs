import TicTacToe

import Test.Hspec
import Text.Printf (printf)

main = hspec $ do
  describe "canary" $ do
    it "truthy" $ do
       True `shouldBe` True

  describe "tic tac toe" $ do
    describe "when it just started" $ do
      it "should be an empty board to start with" $ do
         board game `shouldBe` [[Nothing,Nothing,Nothing], [Nothing,Nothing,Nothing], [Nothing,Nothing,Nothing]]

      it "should have the first player as 'X'" $ do
         currentPlayer game `shouldBe` 'X' 

      it "should not have a winner by default" $ do
         winner game `shouldBe` Nothing

    describe "first move only" $ do
      it "should store the first movement" $ do
        board (makeAMove game (0,0)) `shouldBe` [[Just 'X', Nothing, Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

      it "should flip the current player on first movement" $ do
        currentPlayer (makeAMove game (0,0)) `shouldBe` 'O'

    describe "several movements" $ do
      it "store them" $ do
        let gameAfterFirstMove = makeAMove game (0,0)
        board (makeAMove gameAfterFirstMove (0,1)) `shouldBe` [[Just 'X', Just 'O', Nothing], [Nothing, Nothing, Nothing], [Nothing, Nothing, Nothing]]

      it "flips the current player" $ do
        let gameAfterFirstMove = makeAMove game (0,0)
        let gameAfterSecondMove = makeAMove gameAfterFirstMove (0,1)
        currentPlayer gameAfterSecondMove `shouldBe` 'X'

    describe "a game can be won" $ do
      it "by player X" $ do
        let moves = [(0,0),(2,0),
                     (0,1),(2,1),
                     (0,2)]
        let afterApplyingMoves = foldl (\game move-> makeAMove game move) game moves
        winner afterApplyingMoves `shouldBe` Just 'X'

      it "by player O" $ do
        let moves = [(0,0),(2,0),
                     (1,1),(2,1),
                     (0,2),(2,2)]
        let afterApplyingMoves = foldl (\game move-> makeAMove game move) game moves
        winner afterApplyingMoves `shouldBe` Just 'O'




