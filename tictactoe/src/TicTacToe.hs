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
winnerInHorizontal board = do
  let matchesX = matchesIf3EqualInSameRow board $ Just 'X'
  let matchesO = matchesIf3EqualInSameRow board $ Just 'O'
  [matchesO, matchesX]

winnerInVertical :: Board -> [Maybe Char]
winnerInVertical board = do
  let transposedBoard = transpose board
  let matchesX = matchesIf3EqualInSameRow transposedBoard $ Just 'X'
  let matchesO = matchesIf3EqualInSameRow transposedBoard $ Just 'O'
  [matchesO, matchesX]

matchesIf3EqualInSameRow :: Board -> Maybe Char -> Maybe Char
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

emptyPositions :: Game -> [Movement]
emptyPositions game = do
  let board' = index $ board game
  let flatten xs = foldl (++) [] xs
  let isPresent (movement, play) = case play of 
                                     Nothing -> True 
                                     _ -> False
  let result = map fst $ flatten $ map (\row -> filter isPresent row) board'
  result

--exhaustive :: Game -> [[Movement]]
--exhaustive game = do
--  let boardIsFull game = emptyPositions game == []
--  let flatten xs = foldl (++) [] xs
--  if boardIsFull game then
--    currentMovements
--      else
--        map (\pos->exhaustive (makeAMove game pos)) $ emptyPositions game
 
index :: Board -> [[(Movement, Maybe Char)]]
index board = do
  let indices = [0..2]
  let x = zip indices board
  map (\(rowIndex, row) -> do
    let y = map (\indice -> (rowIndex, indice)) indices
    zip y row) x


board' :: [[Char]] -> [[Maybe Char]]
board' representation = 
  map (\row -> map convert row) representation where
      convert cell =  case cell of
        ' ' -> Nothing
        a -> Just a

