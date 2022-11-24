module Features.Character
  ( -- Types
    Character,
    -- motion
    moveTrack,
    jumpUp,
    -- helper function
    printPos,
  )
where

import Prelude

-- Core types
data Game = Game
  {
    _Runner    :: Track  -- current track (location) of runner
  , _Obstacles :: [Pos]  -- list of obstacles
  , _Jumping   :: Bool   -- is runner jumping now
  , _lastObs   :: Track  -- track of the last obstacle, for generating new one
  , _score     :: Int    -- current score, method TODO
  , _locked    :: Bool   -- game locked during moving/jumping
  , _dead      :: Bool   -- game over
  }

-- Position of obstacle, track and column
type Pos = (Track, Int)

-- Which one of the three tracks(row)
data Track
  = Up
  | Mid
  | Down
  deriving(Eq, Show)

-- Core functions

-------------------------------------------------------------------------------

-- | Character and Motion ---------------------------------------------------

-------------------------------------------------------------------------------

-- Character stores its own postion
data Character = Character
  { pos :: (Int, Int)
  }

test_char :: Character
test_char = Character {pos = (0, 0)}

-- Motion

-- moveUp allow character to change one track up/down, which means add/minus 2 to the row
moveTrack :: Character -> String -> Character
moveTrack character direction = do {
    if direction == "Up" then do {
        curr_row <- fst ( pos character);
        curr_col <- snd ( pos character);
        return Character { pos = (curr_row + 2, curr_col)}
    }
    else if direction == "Down" then do{
        curr_row <- fst ( pos character)
        curr_col <- snd ( pos character)
        return Character { pos = (curr_row - 2, curr_col)}
    }
    else 
        return character
}

-- jumpUp allow character to jump up, which means add 1 to the row
jumpUp :: Character -> Character
jumpUp character = character

testTrack :: Character
track1 = moveTrack test_char "Up"

printPos :: Character -> IO ()
printPos character = putStrLn ("(" ++ show (fst (pos character)) ++ "," ++ show (snd (pos character)) ++ ")")