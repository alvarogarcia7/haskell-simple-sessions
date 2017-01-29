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
  let winnerInHorizontal = case matchesIf3Of board' (Just 'X') of
        Nothing -> matchesIf3Of board' (Just 'O')
        match -> match 
  let winnerInVertical = if ((board' !! 0 !! 0 == Just 'X') && (board' !! 1 !! 0 == Just 'X') && (board' !! 2 !! 0 == Just 'X')) || ((board' !! 0 !! 1 == Just 'X') && (board' !! 1 !! 1 == Just 'X') && (board' !! 2 !! 1 == Just 'X')) || ((board' !! 0 !! 2 == Just 'X') && (board' !! 1 !! 2 == Just 'X') && (board' !! 2 !! 2 == Just 'X')) then Just 'X' else Nothing
  let winByType = [winnerInVertical, winnerInHorizontal]
  last $ sort winByType

matchesIf3Of board movement= 
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

