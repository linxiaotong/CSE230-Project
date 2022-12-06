module Escape
  ( -- Types
    Game,
    Pos,
    Track,
    -- core function
    moveUp,
    moveDown,
    runnerJump,
    -- generateObs,
    updateObs,
    forwardObs,
    removeNeg,
    -- helper function
    printPos,
    test_game,
    test_pos,
    test_track,
    printGame,
  )
where

-- import Control.Monad.Random
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Prelude

-- Core types
data Game = Game
  { _Runner :: Track, -- current track (location) of runner
    _Obstacles :: [Pos], -- list of obstacles
    _Jumping :: Bool, -- is runner jumping now
    _lastObs :: Track, -- track of the last obstacle, for generating new one
    _score :: Int, -- current score, method TODO
    _locked :: Bool, -- game locked during moving/jumping
    _dead :: Bool -- game over
  }

-- Position of obstacle, track and column
type Pos = (Track, Int)

-- Which one of the three tracks(row)
data Track
  = Up
  | Mid
  | Down
  deriving (Eq, Show)

-- Core Function

----------------------------------------------------
--- Step Forward in Time
----------------------------------------------------

-- function to step forward in time, need more time to research on Maybe library

-- Possibly dead if runner track is collide with any of the obstacles, need more investigation too

----------------------------------------------------
--- Runner and Motion
----------------------------------------------------

-- Function to control the Runner to move up one Track, if already in Up, do nothing
moveUp :: Game -> Game
moveUp (Game rn obs jump last score locked dead)
  | rn == Down = Game {_Runner = Mid, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | rn == Mid = Game {_Runner = Up, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | rn == Up = Game {_Runner = Up, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | otherwise = Game {_Runner = Up, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead} -- will this happened?

-- Function to control the Runner to move down one Track, if already in Down, do nothing
moveDown :: Game -> Game
moveDown (Game rn obs jump last score locked dead)
  | rn == Down = Game {_Runner = Down, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | rn == Mid = Game {_Runner = Down, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | rn == Up = Game {_Runner = Mid, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | otherwise = Game {_Runner = Down, _Obstacles = obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead} -- will this happened?

-- Function to change the status of the Runner to Jump (Change True to False, change False to True)
runnerJump :: Game -> Game
runnerJump (Game rn obs jump last score locked dead)
  | jump == True = Game {_Runner = rn, _Obstacles = obs, _Jumping = False, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  | jump == False = Game {_Runner = rn, _Obstacles = obs, _Jumping = True, _lastObs = last, _score = score, _locked = locked, _dead = dead}

----------------------------------------------------
--- Obstacle and Random Generator
----------------------------------------------------

-- Function to generate a Obstacle
-- generateObs :: Pos

-- Function to add the new obstacles to Game (including Obstacles and LastObs)
updateObs :: Pos -> Game -> Game
updateObs pos (Game rn obs jump last score locked dead) = do
  let lastTrack = fst pos
  Game {_Runner = rn, _Obstacles = obs ++ [pos], _Jumping = jump, _lastObs = lastTrack, _score = score, _locked = locked, _dead = dead}

-- Function to change the Pos of the obstalces by moving forward one position
forwardObs :: Game -> Game
forwardObs (Game rn obs jump last score locked dead) = do
  --- forward every obstacles by 1
  let new_obs = map minusOne obs
  let clean_obs = removeNeg new_obs
  Game {_Runner = rn, _Obstacles = clean_obs, _Jumping = jump, _lastObs = last, _score = score, _locked = locked, _dead = dead}
  where
    minusOne (track, pos) = (track, pos - 1)

-- Function to remove the obstacles that is out of the board, which means the pos is negative
removeNeg :: [(Track, Int)] -> [(Track, Int)]
removeNeg [] = []
removeNeg (x : xs) = do
  if (snd x) < 0
    then xs
    else x : removeNeg xs

-- test objects

test_game :: Game
test_game =
  Game
    { _Runner = Mid,
      _Obstacles = [],
      _Jumping = False,
      _lastObs = Mid,
      _score = 0,
      _locked = False,
      _dead = False
    }

test_track :: Track
test_track = Mid

test_pos :: Pos
test_pos = (Mid, 6)

printPos :: Pos -> IO ()
printPos pos = putStrLn (show pos)

printGame :: Game -> IO ()
printGame (Game rn obs jump last score locked dead) = do
  putStrLn ("----------Printing Game Status Right now----------")
  putStrLn ("Runner position is in Track " ++ show rn)
  putStrLn ("Runner Obs list is " ++ show obs)
  putStrLn ("Runner jumping status is " ++ show jump)
  putStrLn ("Runner last Obstacles is in Track " ++ show last)
  putStrLn ("Runner score is " ++ show score)
  putStrLn ("Runner changing track status is locked " ++ show locked)
  putStrLn ("Runner is dead " ++ show dead)
  putStrLn ("----------Finished Printing Game Status-----------")

convertFromM :: Monad m => Game -> m Game
convertFromM game = return (game)
