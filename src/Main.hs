module Main
  ( -- stuff for characters
    Character,
    moveUp,
    printPos,
  )
where

import qualified Features.Character as Character

test_character = Character (0, 0)

printPos test_character