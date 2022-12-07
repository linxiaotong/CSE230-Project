{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

import Test.Tasty
import Common
import Prelude hiding (maximum)
import Features.Board
import Features.Escape
import Features.EnterName

module Main where 

import System.Exit

main :: IO ()
main = do 
  putStrLn "\nRunning my tests... "
  putStrLn "\nDone Testing"
  exitWith ExitSuccess 