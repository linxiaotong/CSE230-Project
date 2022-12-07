{-# LANGUAGE TemplateHaskell #-}

module Features.Escape
  ( -- Types
    Game,
    Pos,
    Track,
    -- Game function
    initGame,
    step,
    die,
    -- core function
    moveUp,
    moveDown,
    runnerJump,
    generateObs,
    forwardObs,
    -- helper function
    printPos,
    test_game,
    test_pos,
    test_track,
    printGame,
  )
where

import Control.Applicative ((<|>))
import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Control.Monad (guard)
-- import Control.Monad.Random

import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
-- import Lens.Micro ((^.))
import System.Random
import Test.QuickCheck
import Text.ParserCombinators.ReadPrec ()
import Prelude

-- Core types
data Game = Game
  { _Runner :: Track, -- current track (location) of runner
    _Obstacles :: [Pos], -- list of obstacles
    _Flags :: Stream Int, --- list of 0/1 as flag to know if the obstacle need to be generate
    _Jumping :: Int, -- is runner jumping now
    _score :: Int, -- current score, method TODO
    _locked :: Bool, -- game locked during moving/jumping
    _dead :: Bool -- game over
  }

-- Position of obstacle, track and column
type Pos = (Track, Int)

data Stream a = a :| Stream a
  deriving (Show)

-- Which one of the three tracks(row)
data Track
  = Up
  | Mid
  | Down
  deriving (Eq, Show)

makeLenses ''Game

-- Core Function

----------------------------------------------------
--- STEP Forward in Time
----------------------------------------------------

-- Function to step forward in time, need more time to research on Maybe library
step :: Game -> Game
step game = fromMaybe game $ do
  -- check die or not
  guard $ not (game ^. dead)
  return $ fromMaybe (takeAction game) (die game)

-- Function of taking action in steps
-- score + 1
-- forwardObs
-- check jumping, if not 0, then -1
-- generated new obstacle

----------------------------------------------------
--- Action of addScore, ForwardObs, and minJump
----------------------------------------------------

takeAction :: Game -> Game
takeAction = (execState generateObs) . addScore . minJump . forwardObs

-- Function of adding given point of score to Game
addScore :: Game -> Game
addScore (Game rn obs flg jump score locked dead) = Game {_Runner = rn, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score + 1, _locked = locked, _dead = dead}

-- Function of checking if the Runner is in Jumping
-- If so, -1, otherwise, keep the same
-- If get to 0, unlocked the state
minJump :: Game -> Game
minJump g@(Game rn obs flg 0 score lock dead) = g
minJump g@(Game rn obs flg 1 score lock dead) = g & jumping .~ 0 & locked .~ False -- if jump is 1, change it back to not jump state and unlock
minJump g@(Game rn obs flg jump score lock dead) = g & jumping .~ (jump - 1)

----------------------------------------------------
--- Obstacle and Random Generator
----------------------------------------------------

-- Function to generate Obstacles
generateObs :: State Game ()
generateObs = do
  -- take one out
  (f :| fs) <- use flags
  obs <- use obstacles
  flags .= fs
  case f of
    1 -> obstacles .= obs ++ [(Down, 15)] -- check if Down track generate obstacle or not
    2 -> obstacles .= obs ++ [(Mid, 15)] -- check if Mid track generate obstacle or not
    3 -> obstacles .= obs ++ [(Up, 15)] -- check if Up track generate obstacle or not
    otherwise -> obstacles .= obs

-- Function to change the Pos of the obstalce by moving forward one position
forwardObs :: Game -> Game
forwardObs (Game rn obs flg jump score locked dead) = do
  --- forward every obstacles by 1
  let new_obs = map minusOne obs
  let clean_obs = removeNeg new_obs
  Game {_Runner = rn, _Obstacles = clean_obs, _Flags = flg, _Jumping = jump, _score = score, _locked = locked, _dead = dead}
  where
    minusOne (track, pos) = (track, pos - 1)

-- Function to add the new obstacles to Game
updateObs :: [Pos] -> Game -> Game
updateObs posList (Game rn obs flg jump score locked dead) = do
  Game {_Runner = rn, _Obstacles = obs ++ posList, _Flags = flg, _Jumping = jump, _score = score, _locked = locked, _dead = dead}

-- Function to remove the obstacles that is out of the board, which means the pos is negative
removeNeg :: [(Track, Int)] -> [(Track, Int)]
removeNeg [] = []
removeNeg (x : xs) = do
  if snd x < 0
    then xs
    else x : removeNeg xs

----------------------------------------------------
--- Check if Game is Over
----------------------------------------------------

-- Possibly dead if runner track is collide with any of the obstacles, need more investigation too
-- execState die game
die :: Game -> Maybe Game
die game = do
  -- collide with any of the obstacles
  guard $ checkDie game -- if checkDie succeed, then dead = True
  return $ game & dead .~ True

-- check if the Game will die
checkDie :: Game -> Bool
checkDie g@(Game rn obs flg jump score locked dead) =
  -- if the runner is not jumping / locked
  if jump > 0
    then False
    else do
      -- not jump, check closest obstacles
      let ob = nextObs g
      if fst ob == rn && snd ob == 0
        then True
        else False

-- if not jumping check track of obstacle in index 0 is the same as runner or not

-- Function to take the obstacle in index 0 (first one in list)
nextObs :: Game -> Pos
nextObs (Game rn [] flg jump score locked dead) = (Mid, -1)
nextObs (Game rn (x : xs) flg jump score locked dead) = x

----------------------------------------------------
--- Runner and Motion
----------------------------------------------------

-- Function to control the Runner to move up one Track, if already in Up, do nothing
moveUp :: Game -> Game
moveUp (Game rn obs flg jump score True dead) = Game {_Runner = rn, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = True, _dead = dead}
moveUp (Game rn obs flg jump score False dead)
  | rn == Down = Game {_Runner = Mid, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Mid = Game {_Runner = Up, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Up = Game {_Runner = Up, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = False, _dead = dead}

-- Function to control the Runner to move down one Track, if already in Down, do nothing
moveDown :: Game -> Game
moveDown (Game rn obs flg jump score True dead) = Game {_Runner = rn, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = True, _dead = dead}
moveDown (Game rn obs flg jump score False dead)
  | rn == Down = Game {_Runner = Down, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Mid = Game {_Runner = Down, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = False, _dead = dead}
  | rn == Up = Game {_Runner = Mid, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = False, _dead = dead}

-- Function to change the status of the Runner to Jump (3)
-- Jumping means locked the runner from moving to other track
-- If locked, then nothing is changed
runnerJump :: Game -> Game
runnerJump (Game rn obs flg jump score True dead) = Game {_Runner = rn, _Obstacles = obs, _Flags = flg, _Jumping = jump, _score = score, _locked = True, _dead = dead}
runnerJump (Game rn obs flg jump score False dead) = Game {_Runner = rn, _Obstacles = obs, _Flags = flg, _Jumping = 2, _score = score, _locked = True, _dead = dead}

----------------------------------------------------
--- INITIALIZATION of the GAME
----------------------------------------------------

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  -- error "fill this in"
  let a = (1, 4) :: (Int, Int)
  -- generated random inifite list here
  fs <- fromList . randomRs a <$> newStdGen
  let g =
        Game
          { _Runner = Mid, -- current track (location) of runner
            _Obstacles = [], -- list of obstacles
            _Flags = fs,
            _Jumping = 0, -- is runner jumping now
            _score = 0, -- current score, method TODO
            _locked = False, -- game locked during moving/jumping
            _dead = False -- game over
          }
  return $ execState generateObs g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

----------------------------------------------------
--- Helper Function for MANUALLY TESTING
----------------------------------------------------

test_game :: Game
test_game =
  Game
    { _Runner = Mid,
      _Obstacles = [],
      _Flags = fromList [1, 2, 1, 3, 1, 4],
      _Jumping = 0,
      _score = 0,
      _locked = False,
      _dead = False
    }

test_track :: Track
test_track = Mid

test_pos :: Pos
test_pos = (Mid, 6)

-- Function to produce an infinite list of pseudo-random values instead of returning a new generator
-- testRan :: IO ()
testRan = do
  g <- newStdGen
  let a = (0, 1) :: (Int, Int)
  print . take 10 $ randomRs a g

printPos :: Pos -> IO ()
printPos pos = putStrLn (show pos)

printGame :: Game -> IO ()
printGame (Game rn obs flg jump score locked dead) = do
  putStrLn "----------Printing Game Status Right now----------"
  putStrLn ("Runner position is in Track " ++ show rn)
  putStrLn ("Runner Obs list is " ++ show obs)
  putStrLn ("Runner jumping status is " ++ show jump)
  putStrLn ("Runner score is " ++ show score)
  putStrLn ("Runner changing track status is locked " ++ show locked)
  putStrLn ("Runner is dead " ++ show dead)
  putStrLn "----------Finished Printing Game Status-----------"

printIO :: IO ()
printIO = do
  g <- initGame
  printGame g