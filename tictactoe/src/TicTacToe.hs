module TicTacToe where

type Board = [[Maybe Char]]
data Game = Game { board::Board }

game :: Game
game = Game {
        board=[[Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing]]
       }

