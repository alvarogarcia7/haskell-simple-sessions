module TicTacToe where

type Board = [[Maybe Char]]
type Movement = (Int, Int)
data Game = Game { board::Board, currentPlayer::Char }

game :: Game
game = Game {
        board=[[Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing]],
        currentPlayer='X'
       }

makeAMove :: Game -> Movement -> Game
makeAMove game movement= Game {
        board = [
            [Just 'X', Nothing, Nothing],
            [Nothing, Nothing, Nothing],
            [Nothing, Nothing, Nothing]]
       }
