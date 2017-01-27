module TicTacToe where

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
  if (any (\row -> [Just 'X', Just 'X', Just 'X'] == row) board') 
    then Just 'X' 
    else if (any (\row -> [Just 'O', Just 'O', Just 'O'] == row) board') then Just 'O'
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

