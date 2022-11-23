module Features.Board
  ( -- Types
    Board,
    -- functions
    checkPos,
    changePos,
    -- helper func
    printBoard,
    getBoardList,
  )
where

import Prelude

-------------------------------------------------------------------------------

-- | Board --------------------------------------------------------------------

-------------------------------------------------------------------------------

-- Board should always be a size of fixed rectangle which has 6 rows (3 tracks) * 15 columns
data Board = Board
  { boardData :: [[Int]]
  }

-- Board size should be 6 * 15
-- All element in Board should be 0
board :: Board
board =
  Board
    { boardData = take 6 (repeat (take 15 (repeat 0)))
    }

-- Given a Position (tuple) and Board object, checking if the Position in Board Object is valid
checkPos :: (Int, Int) -> Board -> Bool
checkPos pos board = True

-- Given a Position (tuple), a Value (Int), and Board, change the board Data in that position to that value
changePos :: (Int, Int) -> Int -> Board -> Board
changePos pos value board = board

-- helper function to print board out
printBoard :: Board -> IO ()
printBoard board = do
  getBoardList 0 (boardData board)
  getBoardList 1 (boardData board)
  getBoardList 2 (boardData board)
  getBoardList 3 (boardData board)
  getBoardList 4 (boardData board)
  getBoardList 5 (boardData board)

-- It takes a Int number (row) and print out that row in a line
getBoardList :: Int -> [[Int]] -> IO ()
getBoardList row bData = do
  if (row >= 0) && (row < (length bData)) == True
    then putStrLn (convertStr (bData !! row))
    else putStrLn "Out of Range"

convertStr :: [Int] -> String
convertStr [] = ""
convertStr (x : xs) = show x ++ "  " ++ convertStr xs