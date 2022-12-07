{-# LANGUAGE OverloadedStrings #-}
module Features.Board where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Features.Escape

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Cube | Obs | Ground | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

playGame :: IO Game
playGame = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 200000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  customMain initialVty builder (Just chan) app g


-- main :: IO ()
-- main = do
--   chan <- newBChan 10
--   forkIO $ forever $ do
--     writeBChan chan Tick
--     threadDelay 100000 -- decides how fast your game moves
--   g <- initGame
--   let builder = V.mkVty V.defaultConfig
--   initialVty <- builder
--   void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ moveUp g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ moveDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'w') [])) = continue $ moveUp g
handleEvent g (VtyEvent (V.EvKey (V.KChar 's') [])) = continue $ moveDown g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ runnerJump g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ drawScore (g ^. score)
         , padTop (Pad 2) $ drawGameOver (g ^. dead)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Cube Escape")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [8, 7..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..15]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` (obsCoord $ g ^. obstacles)  = Obs
      | c == heroCord (g ^. jumping) (g ^. runner)= Cube
      | c `elem` grounds                      = Ground
      | otherwise                             = Empty


heroCord :: Int -> Track -> V2 Int
heroCord j t = if j > 0
  then V2 0 ((trackCord t)+1)
  else V2 0 (trackCord t)

grounds = [V2 y x | x <- [0,3,6], y <- [0..15]]

-- trackCord :: Track -> Int
-- trackCord Up   = 7
-- trackCord Mid  = 4
-- trackCord Down = 1

obsCoord :: [Pos] -> [V2 Int]
obsCoord l = [V2 x y | p <- l, let x = snd p, let y = trackCord (fst p)]

drawCell :: Cell -> Widget Name
drawCell Cube   = withAttr cubeAttr  cw
drawCell Obs    = withAttr obsAttr   cw
drawCell Ground = withAttr gdAttr    cw
drawCell Empty  = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (cubeAttr, V.yellow `on` V.yellow)
  , (obsAttr, V.red `on` V.red)
  , (gdAttr, V.white `on` V.white)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

cubeAttr, obsAttr, gdAttr, emptyAttr :: AttrName
cubeAttr = "cubeAttr"
obsAttr = "obsAttr"
gdAttr = "gdAttr"
emptyAttr = "emptyAttr"
