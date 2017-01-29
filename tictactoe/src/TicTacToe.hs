module TicTacToe where

import Data.List

type Board = [[Maybe Char]]
type Movement = (Int, Int)
data Game = Game { board::Board, currentPlayer::Char, winner::Maybe Char }

game :: Game
game = Game {
        board=[[Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing],
               [Nothing, Nothing, Nothing]],
        currentPlayer='X',
        winner=Nothing
       }

aNewGame :: Board -> Char -> Game
aNewGame board player = Game {board=board, currentPlayer=player, winner=hasWon} where hasWon = Just $ flipPlayer player

hasWon :: Game -> Maybe Char
hasWon game = do
  let board' = board game
  let winnerInHorizontal' = winnerInHorizontal board'
  let winnerInVertical' = winnerInVertical board'
  let winByType = winnerInHorizontal' ++ winnerInVertical'
  last $ sort winByType

winnerInHorizontal :: Board -> [Maybe Char]
winnerInHorizontal board' = do
  let matchesO = matchesIf3EqualInSameRow board' $ Just 'O'
  let matchesX = matchesIf3EqualInSameRow board' $ Just 'X'
  [matchesO, matchesX]

winnerInVertical :: Board -> [Maybe Char]
winnerInVertical board = do
  let transposedBoard = transpose board
  let matchesX = matchesIf3EqualInSameRow transposedBoard $ Just 'X'
  let matchesO = matchesIf3EqualInSameRow transposedBoard $ Just 'O'
  [matchesO, matchesX]


matchesIf3EqualInSameRow board movement= 
  if (any (\row -> [movement,movement,movement] == row) board) 
    then movement 
    else Nothing

makeAMove :: Game -> Movement -> Game
makeAMove Game{board=currentBoard, currentPlayer=currentPlayer} (x,y) = do
    let row = currentBoard !! x 
    let newRow = replaceAt row y (Just currentPlayer)
    let newBoard = replaceAt currentBoard x newRow
    aNewGame newBoard (flipPlayer currentPlayer)

flipPlayer :: Char -> Char
flipPlayer 'X' = 'O'
flipPlayer 'O' = 'X'

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs idx v = let (front, back) = splitAt idx xs in front ++ v:(drop 1 back)

