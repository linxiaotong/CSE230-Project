{-# LANGUAGE ScopedTypeVariables #-}

module Common where

import           Data.IORef
import           Test.Tasty
import           Test.Tasty.HUnit
import           System.Exit
import           System.Process
import           System.IO
import           Control.Exception
import           Text.Printf
import           System.FilePath
import qualified Test.QuickCheck as QC