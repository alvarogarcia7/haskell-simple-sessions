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
makeAMove game movement= do
    let currentBoard = board game
    let row = currentBoard !! (fst movement)
    let newRow = replaceAt row (snd movement) (Just $ currentPlayer game)
    let newBoard = replaceAt currentBoard (fst movement) newRow
    Game {board=newBoard, currentPlayer=TicTacToe.flip $ currentPlayer game}

flip :: Char -> Char
flip 'X' = 'O'
flip 'O' = 'X'

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs idx v = let (front, back) = splitAt idx xs in front ++ v:(drop 1 back)

