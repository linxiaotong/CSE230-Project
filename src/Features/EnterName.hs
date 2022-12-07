{-# LANGUAGE OverloadedStrings #-}

module EnterName(
    enterName,
    getNames
) 
where

import System.Directory

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Control.Monad
import Cursor.Brick.TextField
import Cursor.TextField
import Cursor.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.IO as T
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Path
import Path.IO
import System.Environment
import System.Exit
import Text.Show.Pretty
import qualified System.FilePath as F
import Text.Read (readMaybe)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C

enterName :: IO ()
enterName = do
      fp <- getNameFile
      path <- resolveFile' fp
      initialState <- buildInitialState ""
      endState <- defaultMain editApp initialState
      let contents' = rebuildTextFieldCursor (stateCursor endState)
      T.writeFile (fromAbsFile path) contents'

getNameFile :: IO FilePath
getNameFile = do
  xdg <- getXdgDirectory XdgData "cubeEscapeFooter"
  createDirectoryIfMissing True xdg
  return (xdg F.</> "name")

getNames :: IO String
getNames = do
  lb <- getNameFile
  exists <- System.Directory.doesFileExist lb
  if exists
     then readFile lb
     else return "No exist"

data EditState =
  EditState
    { stateCursor :: TextFieldCursor
    }
  deriving (Show, Eq)

data ResourceName =
  ResourceName
  deriving (Show, Eq, Ord)

editApp :: App EditState e ResourceName
editApp =
  App
    { appDraw = drawEdit
    , appChooseCursor = showFirstCursor
    , appHandleEvent = handleEditEvent
    , appStartEvent = pure
    , appAttrMap = const $ attrMap mempty [("text", fg red), ("bg", fg blue)]
    }

buildInitialState :: Text -> IO EditState
buildInitialState contents = do
  let tfc = makeTextFieldCursor contents
  pure EditState {stateCursor = tfc}

drawEdit :: EditState -> [Widget ResourceName]
drawEdit ts =
  [ 
    forceAttr "text"
    $ padRight (Pad 50)
    $ C.center
    $ vLimit 10
    $ hLimit 50
    $ withBorderStyle BS.unicodeBold
    $ borderWithLabel (str "Enter your Name")
    $ borderWithLabel (str "Press Enter when you finish")
    $ C.center
    $ padLeftRight 1 $ selectedTextFieldCursorWidget ResourceName (stateCursor ts)
    
  ]

handleEditEvent :: EditState -> BrickEvent n e -> EventM n (Next EditState)
handleEditEvent s e =
  case e of
    VtyEvent vtye ->
      let mDo ::
               (TextFieldCursor -> Maybe TextFieldCursor)
            -> EventM n (Next EditState)
          mDo func = do
            let tfc = stateCursor s
            let tfc' = fromMaybe tfc $ func tfc
            let s' = s {stateCursor = tfc'}
            continue s'
       in case vtye of
            EvKey (KChar c) [] -> mDo $ textFieldCursorInsertChar c . Just
            EvKey KUp [] -> mDo textFieldCursorSelectPrevLine
            EvKey KDown [] -> mDo textFieldCursorSelectNextLine
            EvKey KRight [] -> mDo textFieldCursorSelectNextChar
            EvKey KLeft [] -> mDo textFieldCursorSelectPrevChar
                                -- import Cursor.Types
            EvKey KBS [] -> mDo $ dullMDelete . textFieldCursorRemove
            EvKey KDel [] -> mDo $ dullMDelete . textFieldCursorDelete
            EvKey (KChar 'q') [] -> halt s
            EvKey KEnter [] -> halt s
            EvKey KEsc [] -> halt s
            _ -> continue s
    _ -> continue s

