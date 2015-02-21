module Main(main) where

import Control.Monad (forM_, forever)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Data.List (sort, intersperse)

------------------------------------------------------------------------------------------------------------------------

-- Current version
version :: String
version = "0.2"

-- Char to display an empty square on the boad
emptySquare :: Char
emptySquare = '*'

-- Char to display a sqaure on which there are a queen
queenSquare :: Char
queenSquare = 'Q'

------------------------------------------------------------------------------------------------------------------------

{-
Our data type to represent a N x N board.
After all, all that matter is the size of the board and the position of the queens (if any)
For efficiency's sake, we will keep the list of coordinates sorted
-}
data Board = Board {
    boardDim    :: Int              -- N
  , boardQueens :: [(Int, Int)]     -- List of (x, y) coordinates for the queens (origin is at top left)
}

{-
Since there is some constraints (on the coordinates w.r.t. the board's dimension), we provide a way to construct a new
board, that checks the coordinates and ensure queens are not added out-of-bound.
-}
createBoard :: Int -> [(Int, Int)] -> Board
createBoard dim xs = Board dim validQueens
    where isQueenOnBoard n (x, y) = x < n && y < n
          validQueens = sort $ filter (isQueenOnBoard dim) xs

{-
To display a board, here is the idea:
    * we create one string length N x N, containing only the empty characters
    * we traverse the list of coordinates, compute the offset in the string and replace the empty by a queen
    * then we display on screen, breaking the string in strings of length N
-}
displayBoard :: Board -> IO ()
displayBoard (Board n xs) = do
    let str = take (n^2) (repeat emptySquare)
        oneLineCoordinates = map computeOffset xs
        withQueensReplaced = replaceQueens oneLineCoordinates str
        formatted          = breakEveryN withQueensReplaced
    putStrLn formatted
    where computeOffset (x, y) = x * n + y

          replaceQueens xs zs = replaceQueens' (-1) xs zs

          replaceQueens' _ [] zs     = zs
          replaceQueens' currN (y:ys) zs = take (y - currN - 1) zs ++ queenSquare : replaceQueens' y ys (drop (y - currN) zs)

          breakEveryN [] = []
          breakEveryN xs = intersperse ' ' (take n xs) ++ '\n' : breakEveryN (drop n xs)

{-
To check if a board if valid,
    1. check if two queens have same column OR same line
    2. compute the absolute difference between coordinates of each queens and check if there are some doublons
-}
isBoardValid :: Board -> Bool
isBoardValid (Board n xs) =
    let orthogonalCheck  = checkRowAndCols xs
        newCoordinates   = map rotate45 xs
        diagonalCheck    = checkRowAndCols newCoordinates
    in orthogonalCheck && diagonalCheck

    where checkRowAndCols = checkRowAndCols' [] []

          checkRowAndCols' _ _ []           = True
          checkRowAndCols' xs ys ((x,y):cs) | x `elem` xs || y `elem` ys = False
                                            | otherwise                  = checkRowAndCols' (x:xs) (y:ys) cs
          
          rotate45 (x, y) =
            let x' = x - y
                y' = x + y
            in (x', y')
------------------------------------------------------------------------------------------------------------------------

-- Just some greetings + the current version
greetings :: IO ()
greetings = do
    let str = "..::|| LET'S HAVE FUN WITH THE QUEENS! ||::.."
        str2 = take (length str) (repeat '=')
        str3 = "Welcome to the A.I. algorithms comparison v" ++ version
    forM_ [str, str2, str3] putStrLn

-- Present the user with the list of possible actions
displayMenu :: IO ()
displayMenu = do
    let str = "Here are the current supported actions:"
        menu = ["q:\tExit Program."
              , "d:\tDebug"
               ]
    putStrLn str
    forM_ menu (putStrLn . (:) '\t')

-- Take input from stdin, issue action (if matching)
handleInput :: IO ()
handleInput = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        "q"       -> exitProgram
        "d"       -> debug
        otherwise -> wrongInput
------------------------------------------------------------------------------------------------------------------------

-- Oh I'm sure you'll recognize that...
wrongInput :: IO ()
wrongInput =
    putStrLn "I'm sorry, my responses are limited. You must are the right question.\n"
    >> displayMenu
    >> handleInput

-- ... and that too :-)
exitProgram :: IO ()
exitProgram =
    putStrLn "Program. Terminated."
    >> exitSuccess


------------------------------------------------------------------------------------------------------------------------

-- Entry point, present the menu and wait for input
main :: IO ()
main =
    greetings
    >> forever (displayMenu >> handleInput)

debug :: IO ()
debug = do
    let board = createBoard 8 [(4,7), (2,5)]
    displayBoard board
    if (isBoardValid board) then
        putStrLn "Board is valid"
    else
        putStrLn "Board is not valid"