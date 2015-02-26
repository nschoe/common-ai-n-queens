module Main(main) where

import Control.Monad (forM_, forever, when)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Data.List (sort, intersperse, (\\), sortBy)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import System.Random (randomRs, newStdGen)

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
    boardDim    :: Int              -- N (dimension of the board)
  , boardQueens :: [(Int, Int)]     -- List of (x, y) coordinates for the queens (origin is at top left)
}

{-
Since there are some constraints (on the coordinates w.r.t. the board's dimension), we provide a way to construct a new
board, that checks the coordinates and ensures queens are not added out-of-bound.
-}
createBoard :: Int -> [(Int, Int)] -> Board
createBoard dim xs = Board dim validQueens
    where isQueenOnBoard n (x, y) = x < n && y < n
          validQueens = sort $ filter (isQueenOnBoard dim) xs

{-
To display a board, here is the idea:
    * we create one string of length N x N, containing only the empty character
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
-- isBoardValid :: Board -> Bool
-- isBoardValid (Board n xs) =
--     let orthogonalCheck  = checkRowAndCols xs
--         newCoordinates   = map rotate45 xs
--         diagonalCheck    = checkRowAndCols newCoordinates
--     in orthogonalCheck && diagonalCheck

--     where checkRowAndCols = checkRowAndCols' [] []

--           checkRowAndCols' _ _ []           = True
--           checkRowAndCols' xs ys ((x,y):cs) | x `elem` xs || y `elem` ys = False
--                                             | otherwise                  = checkRowAndCols' (x:xs) (y:ys) cs
          
--           rotate45 (x, y) =
--             let x' = x - y
--                 y' = x + y
--             in (x', y')

countAttackingPairs :: Board -> Int
countAttackingPairs (Board n xs) =
    let orthogonalAttacks  = checkRowsAndCols xs
        rotatedCoordinates = map rotate45 xs
        diagonalAttacks    = checkRowsAndCols rotatedCoordinates
    in (orthogonalAttacks + diagonalAttacks)

    where   checkRowsAndCols = checkRowsAndCols' 0 [] []

            checkRowsAndCols' m _ _ [] = m
            checkRowsAndCols' m xs ys ((x,y):cs) | x `elem` xs && y `elem` ys = checkRowsAndCols' (m+2) xs ys cs
                                                | x `elem` xs = checkRowsAndCols' (m+1) xs (y:ys) cs
                                                | y `elem` ys = checkRowsAndCols' (m+1) (x:xs) ys cs
                                                | otherwise   = checkRowsAndCols' m (x:xs) (y:ys) cs

            rotate45 (x,y) =
                let x' = x - y
                    y' = x + y
                in (x',y')

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
        menu = ["r8:\tGenerate random 8x8 board."
              , "hill\tSolve board with hill climbing algorithm"
              , "q:\tExit Program."
              , "d:\tDebug"
               ]
    putStrLn str
    forM_ menu (putStrLn . (:) '\t')

-- Take input from stdin, issue action (if matching)
handleInput :: IORef (Maybe Board) -> IO ()
handleInput board = do
    putStr "> "
    hFlush stdout
    input <- getLine
    case input of
        "r8"      -> generateRandomN board 8
        "hill"    -> hillClimbing board
        "q"       -> exitProgram
        "d"       -> debug board
        otherwise -> wrongInput board
------------------------------------------------------------------------------------------------------------------------

-- Oh I'm sure you'll recognize that...
wrongInput :: IORef (Maybe Board) -> IO ()
wrongInput board =
    putStrLn "I'm sorry, my responses are limited. You must are the right question.\n"
    >> displayMenu
    >> handleInput board

-- ... and that too :-)
exitProgram :: IO ()
exitProgram =
    putStrLn "Program. Terminated."
    >> exitSuccess

-- Generates a random board
generateRandomN :: IORef (Maybe Board) -> Int -> IO ()
generateRandomN board n = do
    randomOffsets <- newStdGen >>= return . randomRs (0,n-1) :: IO [Int]
    let randomPositions = zip randomOffsets [0..(n-1)]
        newBoard = createBoard n randomPositions
    writeIORef board (Just newBoard)
    putStrLn "New board generated"
    when (n <= 20) (displayBoard newBoard)

-- Generate *ALL* successors (be careful of size)
generateAllSuccessors :: Board -> [Board]
generateAllSuccessors board@(Board n xs) =
    concat $ for xs (\pair -> extrudeColumn pair (xs \\ [pair]))

    where
        for = flip map
        extrudeColumn (row,col) cs =
            let thatCol   = [(x,y) | x <- [0..(n-1)] \\ [row], y <- [col]]
                appended  = zipWith (:) thatCol (repeat cs)
                newBoards = map (createBoard n) appended
            in newBoards

------------------------------------------------------------------------------------------------------------------------

hillClimbing :: IORef (Maybe Board) -> IO ()
hillClimbing board = do
    board' <- readIORef board
    case board' of
        Nothing     -> putStrLn "No boad has been generated yet."
        Just b      -> do
            let currentValue = countAttackingPairs b
            if (currentValue == 0) then do
                putStrLn "No need for algorithm, the board is already solved:"
                displayBoard b
            else do
                hillClimbingStep (b, currentValue) board

hillClimbingStep :: (Board, Int) -> IORef (Maybe Board) -> IO ()
hillClimbingStep (stepBoard, stepValue) board = do
    let successors = generateAllSuccessors stepBoard
        withCost = map (\board -> (board, countAttackingPairs board)) successors
        sorted = sortBy (\p1 p2 -> compare (snd p1) (snd p2)) withCost
        best = head sorted
    if (snd best == 0) then do
        putStrLn "Solution found:\n"
        writeIORef board (Just . fst $ best)
        when (boardDim (fst best) <= 20) (displayBoard (fst best))
    else if (snd best < stepValue) then do
        putStrLn "Looping on better state..."
        hillClimbingStep best board
    else if (snd best > stepValue) then do
        putStrLn "Algorithm stopped because only worse states result from this one:"
        when (boardDim stepBoard <= 20) (displayBoard stepBoard)
    else do
        putStrLn $ "Algorithm stopped because the best resulting state has the same value as the current one (" ++ show stepValue ++ ")"

------------------------------------------------------------------------------------------------------------------------

-- Entry point, present the menu and wait for input
main :: IO ()
main = do
    greetings
    currentBoard <- newIORef Nothing
    forever (displayMenu >> handleInput currentBoard)

debug :: IORef (Maybe Board) -> IO ()
debug board = do
    let board = createBoard 8 [(4,0), (5,1), (6,2), (3,3), (4,4), (5,5), (6,6), (5,7)]
    -- let board = createBoard 8 [(0, 3), (3, 0), (7, 3), (3, 7), (3,3)]
    displayBoard board
    -- putStrLn $ "Attacking pairs: " ++ show (countAttackingPairs board)
    -- b <- readIORef board
    -- case b of
    --     Nothing             -> putStrLn "No board has been generated."
    --     Just (currentBoard) -> do
    --         putStrLn "Current Board: "
    --         displayBoard currentBoard
    --         putStrLn "\nNow generating all successors (1st column):"
    --         let succs = generateAllSuccessors currentBoard
    --             values = map countAttackingPairs succs
    --         forM_ values print