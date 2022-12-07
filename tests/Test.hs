{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Common
import Features.Board
import Features.EnterName
import Features.Escape
import Test.Tasty
import Prelude hiding (maximum)

--import System.Exit

main :: IO ()
main = do
  putStrLn "\nRunning my tests... "
  basicG <- initGame
  runTests [probCore basicG,
            probEnd  basicG]
  putStrLn "\nDone Testing"

--exitWith ExitSuccess

probCore :: Game -> Score -> TestTree
probCore g sc =
  testGroup
    "MoveCore"
    [ scoreTest (move1, (moveUp g), 7, 1, "Up1"),
      scoreTest (move1, (moveUp $ moveUp g), 7, 1, "Up2"),
      scoreTest (move1, (moveUp $ runnerJump g), 4, 1, "Up3"),
      scoreTest (move1, (moveDown g), 1, 1, "Down1"),
      scoreTest (move1, (moveDown $ moveDown g), 1, 1, "Down2"),
      scoreTest (move1, (moveDown $ runnerJump g), 4, 1, "Down3"),
      scoreTest (jump1, (runnerJump g), 2, 1, "Jump1")
    ]
  where
    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    move1 :: Game -> IO Int
    move1 g@(Game rn obs flg jump score lock dead) = return $ trackCord (rn)
    jump1 :: Game -> IO Int
    jump1 g@(Game rn obs flg jump score lock dead) = return jump
-- =======
probEnd g sc =
  testGroup
    "EndCore"
    [ scoreTest (end1, (step $ g {_Obstacles = [(Mid,0)]}), True, 1, "Up1"), 
      scoreTest (end1, (step $ g {_Runner = Up,_Obstacles = [(Up,0)]}), True, 1, "Up1")
    ]
  where
    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    end1 :: Game -> IO Bool
    end1 g@(Game rn obs flg jump score lock dead) = return $ dead