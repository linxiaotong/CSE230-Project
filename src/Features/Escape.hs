module Features.Escape
  ( -- Types
    Game,
    Pos,
    Track,
    -- Game function
    initGame,
    -- step,
    -- dead,
    -- core function
    moveUp,
    moveDown,
    runnerJump,
    -- generateObs,
    trackObs,
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

import Control.Monad (guard)
import Control.Monad.Random
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Random
import Test.QuickCheck
import Text.ParserCombinators.ReadPrec ()
import Prelude

-- Core types
data Game = Game
  { _Runner :: Track, -- current track (location) of runner
    _Obstacles :: [Pos], -- list of obstacles
    _Jumping :: Bool, -- is runner jumping now
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
step :: Game -> Game
step g = fromMaybe g $ do
  guard (not (g ^. dead))
  -- unlock from last
  MaybeT . fmap Just (locked .= False)
  --
  die <|> MaybeT (Just <$> modify moveUp)
    <|> MaybeT (Just <$> modify moveDown)
    <|> MaybeT (Just <$> modify runnerJump)

-- Possibly dead if runner track is collide with any of the obstacles, need more investigation too
die :: MaybeT (State Game) ()
die = do
  error "fill this in" --collide with any of the obstacles
  MaybeT . fmap Just $ dead .= True

----------------------------------------------------
--- Runner and Motion
----------------------------------------------------

-- Function to control the Runner to move up one Track, if already in Up, do nothing
moveUp :: Game -> Game
moveUp (Game rn obs jump score True dead) = Game {_Runner = rn, _Obstacles = obs, _Jumping = jump, _score = score, _locked = True, _dead = dead}
moveUp (Game rn obs jump score False dead)
  | rn == Down = Game {_Runner = Mid, _Obstacles = obs, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Mid = Game {_Runner = Up, _Obstacles = obs, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Up = Game {_Runner = Up, _Obstacles = obs, _Jumping = jump, _score = score, _locked = False, _dead = dead}

-- Function to control the Runner to move down one Track, if already in Down, do nothing
moveDown :: Game -> Game
moveDown (Game rn obs jump score True dead) = Game {_Runner = rn, _Obstacles = obs, _Jumping = jump, _score = score, _locked = True, _dead = dead}
moveDown (Game rn obs jump score False dead)
  | rn == Down = Game {_Runner = Down, _Obstacles = obs, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Mid = Game {_Runner = Down, _Obstacles = obs, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Up = Game {_Runner = Mid, _Obstacles = obs, _Jumping = jump, _score = score, _locked = False, _dead = dead}

-- Function to change the status of the Runner to Jump (Change True to False, change False to True)
-- Jumping means locked the runner from moving to other track
runnerJump :: Game -> Game
runnerJump (Game rn obs jump score locked dead)
  | jump == True = Game {_Runner = rn, _Obstacles = obs, _Jumping = False, _score = score, _locked = False, _dead = dead}
  | jump == False = Game {_Runner = rn, _Obstacles = obs, _Jumping = True, _score = score, _locked = True, _dead = dead}

----------------------------------------------------
--- Obstacle and Random Generator
----------------------------------------------------

-- Function to generate a 0 / 1 as flag to decided create Obs or not
genFlag :: IO Int
genFlag = getStdRandom $ randomR (0, 1 :: Int)

-- Function to generate a list of Obs with given 3 track and position 15(last cell of the board)
-- For each track if the random number is 1, then it generate a pos, otherwise not generate a list
generateObs :: IO ()
generateObs = do
  trackObs Down
  trackObs Mid
  trackObs Up

-- Function of decided whether a track is generated a obstacles
trackObs :: Track -> IO ()
trackObs track = do
  flag <- genFlag
  if flag == 1
    then print (track, 15)
    else print (track, -1)

-- Function to add the new obstacles to Game (including Obstacles and LastObs)
updateObs :: [Pos] -> Game -> Game
updateObs posList (Game rn obs jump score locked dead) = do
  Game {_Runner = rn, _Obstacles = obs ++ posList, _Jumping = jump, _score = score, _locked = locked, _dead = dead}

-- Function to change the Pos of the obstalce by moving forward one position
forwardObs :: Game -> Game
forwardObs (Game rn obs jump score locked dead) = do
  --- forward every obstacles by 1
  let new_obs = map minusOne obs
  let clean_obs = removeNeg new_obs
  Game {_Runner = rn, _Obstacles = clean_obs, _Jumping = jump, _score = score, _locked = locked, _dead = dead}
  where
    minusOne (track, pos) = (track, pos - 1)

-- Function to remove the obstacles that is out of the board, which means the pos is negative
removeNeg :: [(Track, Int)] -> [(Track, Int)]
removeNeg [] = []
removeNeg (x : xs) = do
  if snd x < 0
    then xs
    else x : removeNeg xs

----------------------------------------------------
--- Score
----------------------------------------------------

-- Function of adding given point of score to Game
addScore :: Int -> Game -> Game
addScore point (Game rn obs jump score locked dead) = Game {_Runner = rn, _Obstacles = obs, _Jumping = jump, _score = score + point, _locked = locked, _dead = dead}

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  error "fill this in"
  let g =
        Game
          { _Runner = Mid, -- current track (location) of runner
            _Obstacles = error "fill this in", -- list of obstacles
            _Jumping = False, -- is runner jumping now
            _score = 0, -- current score, method TODO
            _locked = False, -- game locked during moving/jumping
            _dead = False -- game over
          }
  return g

-- test objects

test_game :: Game
test_game =
  Game
    { _Runner = Mid,
      _Obstacles = [],
      _Jumping = False,
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
printGame (Game rn obs jump score locked dead) = do
  putStrLn "----------Printing Game Status Right now----------"
  putStrLn ("Runner position is in Track " ++ show rn)
  putStrLn ("Runner Obs list is " ++ show obs)
  putStrLn ("Runner jumping status is " ++ show jump)
  putStrLn ("Runner score is " ++ show score)
  putStrLn ("Runner changing track status is locked " ++ show locked)
  putStrLn ("Runner is dead " ++ show dead)
  putStrLn "----------Finished Printing Game Status-----------"
