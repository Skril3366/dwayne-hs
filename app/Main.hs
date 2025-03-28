{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Brick
import Brick.Widgets.Center
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (..))
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Model.OrgMode
import Parser.OrgParser
import Parser.Parser (ParserResult (..), runParser)
import Render.OrgRender ()
import qualified Render.Render as R
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import Writer.OrgWriter
import Writer.Writer

import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Border.Style (unicodeRounded)
import GHC.Base (when)
import System.IO
import System.Process
import TextUtils

-- TODO: I should go through the code, collect all the errors and create widget
-- to properly display them

-- TODO: First screen to make:
-- On the left display tasks on single line each (like in emacs) and on the
-- right show focused task in full
-- I think that it would allow for fast parsing of inbox
-- Also add shortcuts for changing the state of the task and so on
-- Also I can list all the subtasks indented under the given task in this
-- one-line view and show on the left as well focused task (or I can try to
-- experiment and show the project task and all of its subtasks at once)

data AppState = CompactMode | FullMode

data AppContext = AppContext
  { tasks :: [Task]
  , currentCursor :: Int
  , appState :: AppState
  }

data AppShortcut = SimpleShortcut {key :: Char, modifyState :: AppContext -> AppContext}

app :: App AppContext e ()
app =
  App
    { appDraw = drawUI -- List in type signature because each element is a layer and thus you can put widgets on top of one another
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return ()
    , appAttrMap = const theAppAttrMap
    }

shortcuts :: [AppShortcut]
shortcuts =
  [ SimpleShortcut 'k' (\s -> s{currentCursor = max 0 (currentCursor s - 1)})
  ]

highlightAttr :: AttrName
highlightAttr = attrName "highlight"

theAppAttrMap :: AttrMap
theAppAttrMap =
  attrMap
    defAttr
    [ (highlightAttr, fg yellow) -- Set foreground to yellow
    ]

handleEvent :: BrickEvent () e -> EventM () AppContext ()
handleEvent (VtyEvent (EvKey (KChar 'k') [])) = modify (\s -> s{currentCursor = max 0 (currentCursor s - 1)}) -- Move up
handleEvent (VtyEvent (EvKey (KChar 'j') [])) = modify (\s -> s{currentCursor = min (currentCursor s + 1) (length (tasks s) - 1)}) -- Move down
handleEvent (VtyEvent (EvKey (KChar 'q') [])) = halt -- Exit application
handleEvent (VtyEvent (EvKey KEnter [])) = do
  state <- get
  when (null (tasks state)) $ return ()
  let currentTask = tasks state !! currentCursor state
  suspendAndResume $ do
    editedContent <- editWithEditor (write currentTask)
    when (null editedContent) $ return ()
    case editedContent of
      Nothing -> return state
      Just editedStr -> do
        let (_, _, result) = runParser anyTaskparser (T.pack editedStr)
        case result of
          ParserSuccess t -> do
            let updatedTasks = take (currentCursor state) (tasks state) ++ [t] ++ drop (currentCursor state + 1) (tasks state)
            return state{tasks = updatedTasks}
          ParserFailure e -> do
            putStrLn $ "Parser error: " ++ show e -- TODO: show error in UI
            return state
handleEvent _ = return () -- Ignore other events

editWithEditor :: T.Text -> IO (Maybe String)
editWithEditor content = do
  editor <- fmap (fromMaybe "vim") (lookupEnv "EDITOR")
  (tempPath, tempHandle) <- openTempFile "/tmp" "edit.txt"
  hPutStr tempHandle (T.unpack content)
  hFlush tempHandle
  hClose tempHandle
  exitCode <- system (editor ++ " " ++ tempPath)
  case exitCode of
    ExitSuccess -> do
      newContent <- readFile tempPath >>= \c -> length c `seq` return c
      removeFile tempPath
      return (Just newContent)
    _ -> do
      removeFile tempPath
      return Nothing

ui :: String -> Widget ()
ui text = vBox [hCenter $ str "Top widget", center $ txtWrap (T.pack text)]

drawUI :: AppContext -> [Widget ()]
drawUI (AppContext ts cursor appState) = case appState of
  FullMode ->
    [hCenter $ str "Top widget", hCenter $ vCenter renderedTask]
   where
    renderedTask = R.renderFull $ ts !! cursor
  CompactMode -> drawCompactListView cursor ts

-- TODO: implement scrolling, currently cursor goes out of the screen when going
-- down
drawCompactListView :: Int -> [Task] -> [Widget ()]
drawCompactListView cursor ts = [joinBorders $ withBorderStyle unicodeRounded $ hBox [hLimitPercent 50 $ hBox [vBox withHighlight, fill ' '], vBorder, focusedTask]]
 where
  withHighlight = zipWith (\i x -> if i == cursor then withAttr highlightAttr x else x) [0 ..] simplyRendered
  simplyRendered = fmap R.renderCompact ts
  focusedTask = R.renderFull (ts !! cursor)

main :: IO ()
main = do
  content <- readFileExample "./resources/Phone.org"
  let (_, _, tasks) = runParser orgFileParser content
  case tasks of
    ParserSuccess (TaskFile _ tasks) -> void $ defaultMain app (AppContext tasks 0 CompactMode)
    -- NOTE: useful code below to save file
    -- ParserSuccess (TaskFile name tasks) -> do
    -- let wrote = write (TaskFile name tasks)
    -- void $ writeFileExample "./resources/parsed.org" wrote
    ParserFailure e -> simpleMain (ui (show e))
