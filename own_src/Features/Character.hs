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