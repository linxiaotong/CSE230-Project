{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Common
import Features.Board
import Features.EnterName
import Features.Escape
import Test.Tasty
import Prelude hiding (maximum)
import qualified Data.Text.IO as T
import qualified Data.Text as TX
import Path
import Path.IO

--import System.Exit

main :: IO ()
main = do
  putStrLn "\nRunning my tests... "
  basicG <- initGame
  runTests [probCore basicG, probCore2 "test", probEnd basicG]
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

probCore2 :: String -> Score -> TestTree
probCore2 str sc = testGroup "EnterName" [
  scoreTest (prop_getNameFile_getNames, str, True, 1, "prop_getNameFile_getNames")
  ]
  where

    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    prop_getNameFile_getNames :: String -> IO Bool
    prop_getNameFile_getNames s = do
        fp <- getNameFile
        path <- resolveFile' fp
        T.writeFile (fromAbsFile path) (TX.pack s)
        name <- getNames
        return (name == s)

probEnd :: Game -> Score -> TestTree
probEnd g sc =
  testGroup
    "EndCore"
    [ scoreTest (end1, (step $ g {_Obstacles = [(Mid,0)]}), True, 1, "End1"), 
      scoreTest (end1, (step $ g {_Runner = Up,_Obstacles = [(Up,0)]}), True, 1, "End2"),
      scoreTest (end1, (step $ g {_Runner = Down,_Obstacles = [(Down,0)]}), True, 1, "End3")
    ]
  where
    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    end1 :: Game -> IO Bool
    end1 g@(Game rn obs flg jump score lock dead) = return $ dead

