module Main(main) where

import Control.Monad (forM_)
import qualified Data.Vector as V

------------------------------------------------------------------------------------------------------------------------

-- A square on our grid can be either empty or there might be a queen on it
data Square = Empty | Queen
    deriving (Eq)

instance Show Square where
    show Empty = "_"
    show Queen = "x"

------------------------------------------------------------------------------------------------------------------------

data Grid = Grid {
    gridDim  :: Int
  , gridGrid :: V.Vector (V.Vector Square)
} deriving (Eq)

-- Grid must be N x N squares, so provide a function to create a new Grid
newGrid :: Int -> Grid
newGrid n = let grid = V.replicate n (V.replicate n Empty)
            in Grid {
            gridDim  = n
          , gridGrid = grid
        }

-- Some standard grids to test algorithms on
-- Standard 8x8 grid
standard8Grid :: Grid
standard8Grid = newGrid 8

-- Standard 100x100 grid
standard100Grid :: Grid
standard100Grid = newGrid 100

-- Standard 1000x1000 grid
standard1kGrid :: Grid
standard1kGrid = newGrid 1000

-- Standard 1million x 1million grid
standard1mGrid :: Grid
standard1mGrid = newGrid 1000000

displayGrid :: Grid -> IO ()
displayGrid (Grid n rows) = do
    putStr "  "
    forM_ [0..(n-1)] $ \nbCol -> putStr $ show nbCol ++ " "
    putStrLn ""
    V.foldM_ (\n v -> displayRowN n v) 0 rows

displayRowN :: Int -> V.Vector Square -> IO Int
displayRowN n row = do
    putStr (show n ++ " ")
    V.forM_ row $ \sq' -> putStr $ show sq' ++ " "
    putStrLn ""
    return (n+1)

------------------------------------------------------------------------------------------------------------------------

insertPiece :: Square -> (Int, Int) -> Grid -> Grid
insertPiece sq (x, y) g@(Grid n rows) | x >= n || y >= n = g
                                   | otherwise        =
                                        let rowY = rows V.! y
                                            newRowY = rowY V.// [(x, sq)]
                                        in g {gridGrid = (rows V.// [(y, newRowY)])}

insertQueen :: (Int, Int) -> Grid -> Grid
insertQueen = insertPiece Queen

removeQueen :: (Int, Int) -> Grid -> Grid
removeQueen = insertPiece Empty

-- isGridValid :: Grid -> Bool
-- isGridValid g@(Grid n rows) = flip V.map rows $ \row -> V.foldl countQueensOnRow 0

------------------------------------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let s = standard8Grid
    displayGrid s
    putStrLn "\nInserting Queen in (2,5)"
    let s' = insertQueen (2, 5) s
    displayGrid s'
    putStrLn "\nRemoving Queen in (2,5)"
    let s'' = removeQueen (2, 5) s'
    displayGrid s''