module Main where

import Features.Board (playGame)
import Features.Escape 
import Features.EnterName(enterName, getNames)

import Control.Monad (when)
import Data.Monoid ((<>))
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

import Control.Lens hiding ((:<), (:>), (<|), (|>))
import Options.Applicative
import qualified System.Directory as D
import System.FilePath ((</>))

newtype Opts = Opts { sco :: Bool }

opts :: Parser Opts
opts = Opts
  <$> switch (long "high-score" <> short 's' <> help "Print highscore record and exit")

cubeEscapeHeader :: String
cubeEscapeHeader = "Cube Escape - Do your Best to get higher score!"

cubeEscapeFooter :: String
cubeEscapeFooter = "Controls - WS or arrow keys(up & down) to change track.\n Space to jump, q or esc to quit"

fullOpts :: ParserInfo Opts
fullOpts = info (helper <*> opts) (fullDesc <> header cubeEscapeHeader <> footer cubeEscapeFooter)


main :: IO ()
main = do
  (Opts hs) <- execParser fullOpts
  when hs (getHighScore >>= printRecord >> exitSuccess) -- show high score and exit
  enterName
  g <- playGame
  let scc = g ^. score
  handleEndGame (scc)


handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = do 
      name <- getNames
      let newName = if name == "" then "No name" else name
      putStrLn $ id newName ++ "'s final score: " ++ show s
    newHighScore = do
      name <- getNames
      let newName = if name == "" then "No name" else name
      putStrLn $ "Congrats! " ++ id newName ++ "! New Record: " ++ show s
      setHighScore s

-- High score stuff
getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getRecordFile
  exists <- D.doesFileExist lb
  if exists
     then readMaybe <$> readFile lb
     else return Nothing

setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getRecordFile
  writeFile lb (show s)


getRecordFile :: IO FilePath
getRecordFile = do
  xdg <- D.getXdgDirectory D.XdgData "cubeEscape"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "score")

-- Utilities
printRecord :: Show a => Maybe a -> IO ()
printRecord Nothing  = putStrLn "None"
printRecord (Just s) = do 
  name <- getNames
  let newName = if name == "" then "No name" else name
  putStrLn $ id newName ++ ": " ++ show s
