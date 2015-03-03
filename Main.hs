module Main(main) where

import Control.Monad (forM_, forever, when)
import System.IO (hFlush, stdout)
import System.Exit (exitSuccess)
import Data.List (sort, intersperse, (\\), sortBy)
import Data.IORef (newIORef, writeIORef, readIORef, IORef)
import System.Random (randomRs, newStdGen, randomR)
import Control.Concurrent (forkIO, killThread)
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
    let str = "\nHere are the current supported actions (\"_s\" means non-verbose (silent) version):"
        menu = ["r8:\t\tGenerate random 8x8 board."
              , "r:\t\tGenerate random NxN board (N is asked)"
              , "hill(_s):\tSolve board with hill-climbing algorithm"
              , "hill_rr(_s):\tSolve board with hill-climbing algorithm with random restart"
              , "stats_hill:\tRun stats on the hill-climbing algorithm"
              , "stats_hill_rr:\tRun stats on the hill-climbing algorithm with random restart"
              , "q:\t\tExit Program."
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
        "r8"            -> generateRandomN True board 8
        "r"             -> askDim >>= generateRandomN True board
        "hill"          -> hillClimbing True board >> return ()
        "hill_s"        -> hillClimbing False board >> return ()
        "hill_rr"       -> hillClimbingRandomRestart True board >> return ()
        "hill_rr_s"     -> hillClimbingRandomRestart False board >> return ()
        "stats_hill"    -> askDim >>= runStats hillClimbing board
        "stats_hill_rr" -> askDim >>= runStats hillClimbingRandomRestart board
        "q"             -> exitProgram
        otherwise       -> wrongInput board
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

askDim :: IO Int
askDim = do
    putStr "What dimension for the board(s) [8]?\n> " >> hFlush stdout
    rawInput <- getLine
    let dim = if rawInput == "" then 8 else read rawInput
    return dim

-- Generates a random board
generateRandomN :: Bool -> IORef (Maybe Board) -> Int -> IO ()
generateRandomN verbose board n = do
    randomOffsets <- newStdGen >>= return . randomRs (0,n-1) :: IO [Int]
    let randomPositions = zip randomOffsets [0..(n-1)]
        newBoard = createBoard n randomPositions
    writeIORef board (Just newBoard)
    when verbose $ do
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

hillClimbing :: Bool -> IORef (Maybe Board) -> IO (Maybe (Bool, Int))
hillClimbing verbose board = do
    board' <- readIORef board
    case board' of
        Nothing     -> do
            when verbose $
                putStrLn "No board has been generated yet."
            return Nothing
        Just b      -> do
            let currentValue = countAttackingPairs b
            if (currentValue == 0) then do
                when verbose $ do
                    putStrLn "No need for algorithm, the board is already solved:"
                    displayBoard b
                return $ Just (True, 0)
            else do
                (succeeded, nbOfSteps) <- hillClimbingStep verbose 0 100 (b, currentValue) board
                when verbose $ do
                    if succeeded then do
                        putStrLn $ "Found a solution in " ++ show nbOfSteps ++ " steps."
                    else do
                        putStrLn $ "Blocked on a solution after " ++ show nbOfSteps ++ " steps."
                return $ Just (succeeded, nbOfSteps)

hillClimbingStep :: Bool -> Int -> Int -> (Board, Int) -> IORef (Maybe Board) -> IO (Bool, Int)
hillClimbingStep verbose moveNb lateralLimit (stepBoard, stepValue) board = do
    let successors = generateAllSuccessors stepBoard
        withCost = map (\board -> (board, countAttackingPairs board)) successors
        sorted = sortBy (\p1 p2 -> compare (snd p1) (snd p2)) withCost
        best = head sorted
    if (snd best == 0) then do
        writeIORef board (Just . fst $ best)
        when verbose $ do
            putStrLn "Solution found:\n"
            when (boardDim (fst best) <= 20) (displayBoard (fst best))
        return (True, moveNb)
    else if (snd best < stepValue) then do
        when verbose $
            putStrLn "Looping on better state..."
        hillClimbingStep verbose (moveNb + 1) lateralLimit best board
    else if (snd best > stepValue) then do
        when verbose $ do
            putStrLn "Algorithm stopped because only worse states result from this one:"
            when (boardDim stepBoard <= 20) (displayBoard stepBoard)
        return (False, moveNb)
    else do
        if lateralLimit > 0 then do
            when verbose $ do
                putStrLn "Making a (random) lateral move..."
            let bestOnPalier = takeWhile ((==) stepValue . snd) sorted
            (offset, _) <- newStdGen >>= return . randomR (0, length bestOnPalier - 1)
            hillClimbingStep verbose (moveNb + 1) (lateralLimit - 1) (bestOnPalier !! offset) board
        else do
            when verbose $ do
                putStrLn "Algorithm stopped because it reached maximum of lateral moves"
            return (False, moveNb)

hillClimbingRandomRestart :: Bool -> IORef (Maybe Board) -> IO (Maybe (Bool, Int))
hillClimbingRandomRestart verbose board = do
    board' <- readIORef board
    case board' of
        Nothing     -> do
            when verbose $
                putStrLn "No board has been generated yet."
            return Nothing
        Just b      -> do
            let currentValue = countAttackingPairs b
            if (currentValue == 0) then do
                when verbose $ do
                    putStrLn "No need for algorithm, the board is already solved:"
                    displayBoard b
                return $ Just (True, 0)
            else do
                hillClimbingRandomRestart' 0 verbose board >>= return . Just

hillClimbingRandomRestart' :: Int -> Bool -> IORef (Maybe Board) -> IO (Bool, Int)
hillClimbingRandomRestart' stepNb verbose board = do
    Just b <- readIORef board
    let currentValue = countAttackingPairs b
    (succeeded, nbOfSteps) <- hillClimbingStep verbose 0 100 (b, currentValue) board
    when verbose $ do
        if succeeded then do
            putStrLn $ "Found a solution in " ++ show nbOfSteps ++ " steps."
        else do
            putStrLn $ "Solution not reached after " ++ show nbOfSteps ++ ", restarting on random state."
    if succeeded then
        return (True, stepNb + nbOfSteps)
    else do
        generateRandomN verbose board 8
        hillClimbingRandomRestart' (stepNb + nbOfSteps) verbose board

------------------------------------------------------------------------------------------------------------------------

{-
Takes an algorithm as a parameter, asks the user how many boards to run the stats on and compute
-}
runStats :: (Bool -> IORef (Maybe Board) -> IO (Maybe (Bool, Int))) -> IORef (Maybe Board) -> Int -> IO ()
runStats algo board dim = do
    -- Ask the user how many steps he wants to run the stats on
    putStr "How many steps do you want to run [100]?\n> " >> hFlush stdout
    rawInput <- getLine
    let howMany = if rawInput == "" then 100 else read rawInput
    -- tId <- forkIO (runStats' algo howMany)
    putStrLn $ "Launching stats on " ++ show howMany ++ " computations for " ++ show dim ++ "x" ++ show dim ++ " boards."
    runStats' algo howMany dim

    where
        runStats' algo howMany dim = do
            (nbSucceeded, stepsToSucceed, stepsToFail) <- runStats'' 0 0 0 algo howMany dim
            let percentage = fromInteger nbSucceeded / fromInteger howMany * 100 :: Double
                avgOK = fromIntegral stepsToSucceed / fromIntegral nbSucceeded :: Double
                avgKO = fromIntegral stepsToFail / fromIntegral (howMany - nbSucceeded) :: Double
            putStrLn "\n###########################################################"
            putStrLn $ "Solved " ++ show nbSucceeded ++ "/" ++ show howMany ++ " (" ++ take 4 (show percentage) ++ "%)."
            putStrLn $ "It takes an average " ++ show (take 4 (show avgOK)) ++ " steps to find a solution."
            putStrLn $ "It takes an average " ++ show (take 4 (show avgKO)) ++ " steps to fail."
            putStrLn "###########################################################\n"

        runStats'' nbSucceeded stepsToSucceed stepsToFail algo 0 _ = do
            return (nbSucceeded, stepsToSucceed, stepsToFail)
        runStats'' nbSucceeded stepsToSucceed stepsToFail algo remaining dim = do
            generateRandomN False board dim
            b <- readIORef board
            Just (solved, steps) <- algo False board
            if solved then
                runStats'' (nbSucceeded + 1) (stepsToSucceed + steps) stepsToFail algo (remaining - 1) dim
            else
                runStats'' nbSucceeded stepsToSucceed (stepsToFail + steps) algo (remaining - 1) dim

------------------------------------------------------------------------------------------------------------------------

-- Entry point, present the menu and wait for input
main :: IO ()
main = do
    greetings
    currentBoard <- newIORef Nothing
    forever (displayMenu >> handleInput currentBoard)