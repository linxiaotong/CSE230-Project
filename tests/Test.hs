{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)
import Features.Board
import Features.Escape
import Features.EnterName


--import System.Exit

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  basicG <- initGame
  runTests [probCore basicG]
  putStrLn "\nDone Testing"
  --exitWith ExitSuccess 

probCore :: Game -> Score -> TestTree
probCore g sc = testGroup "MoveCore" [
  scoreTest (move1, g, 7, 1, "Up1")
  -- scoreTest ((\_ -> checkImage "chess1.png"    mkChess1),     (), True, 2, "chess-1"),
  -- scoreTest ((\_ -> checkImage "chess2.png"    mkChess2),     (), True, 2, "chess-2"),
  -- scoreTest ((\_ -> checkImage "triangle1.png" mkTriangle1),  (), True, 3, "triangle-1"),
  -- scoreTest ((\_ -> checkImage "triangle2.png" mkTriangle2),  (), True, 3, "triangle-2"),
  -- scoreTest ((\_ -> checkImage "carpet.png"    mkCarpet),     (), True, 5, "carpet")
  ]
  where 
    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    move1 :: Game -> IO Int
    move1 g = return $ trackCord(runner (moveUp g))

-- basicG = Game { _Runner = Mid, -- current track (location) of runner
--     _Obstacles :: [], -- list of obstacles
--     _Flags :: Stream Int, --- list of 0/1 as flag to know if the obstacle need to be generate
--     _Jumping :: Int, -- is runner jumping now
--     _score :: Int, -- current score, method TODO
--     _locked :: Bool, -- game locked during moving/jumping
--     _dead :: Bool -- game over
--   }