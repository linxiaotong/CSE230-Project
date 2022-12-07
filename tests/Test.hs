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
  runTests [probCore basicG]
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
      scoreTest (move1, (moveDown $ runnerJump g), 4, 1, "Down3")
    ]
  where
    scoreTest :: (Show b, Eq b) => (a -> IO b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (f, x, r, n, msg)
    move1 :: Game -> IO Int
    move1 g@(Game rn obs flg jump score lock dead) = return $ trackCord (rn)

-- basicG = Game { _Runner = Mid, -- current track (location) of runner
--     _Obstacles :: [], -- list of obstacles
--     _Flags :: Stream Int, --- list of 0/1 as flag to know if the obstacle need to be generate
--     _Jumping :: Int, -- is runner jumping now
--     _score :: Int, -- current score, method TODO
--     _locked :: Bool, -- game locked during moving/jumping
--     _dead :: Bool -- game over
--   }