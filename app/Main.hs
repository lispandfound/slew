{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay)
import System.Process (readCreateProcess, shell)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import Brick
import Brick.BChan as BC
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.Edit
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Text.Printf (printf)
import Control.Monad (void)
import Control.Lens hiding (zoom)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Model.SQueue (jobs)
import Model.Job
import qualified UI.Transient as TR

import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Util (fg)
import Data.Text (Text, pack)
import Data.Maybe (fromMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Time.Format as TF 
import GHC.Generics (Generic)
import UI.Transient (handleTransientEvent)


-- Helper to format Unix timestamps to human-readable time
formatUnixTime :: Int -> Text
formatUnixTime unixTime =
  pack $ TF.formatTime TF.defaultTimeLocale "%Y-%m-%d %H:%M:%S" (posixSecondsToUTCTime (fromIntegral unixTime))

-- Helper to display Maybe Int
displayMaybeInt :: Maybe Int -> Text
displayMaybeInt = fromMaybe "N/A" . fmap (pack . show)

-- Helper to display time limit
displayTimeLimit :: Int -> Text
displayTimeLimit t
  | t == 0 = "UNLIMITED"
  | otherwise = pack $ show t ++ " min" -- Slurm time limits are often in minutes

-- Function to draw the job summary panel
drawJobPanel :: Job -> Widget Name
drawJobPanel job =
  let
    -- Helper for labeled fields
    labeledField :: Text -> Text -> Widget Name
    labeledField label value =
      hBox [ withAttr (attrName "jobLabel") (txt (label <> ": "))
           , withAttr (attrName "jobValue") (txt value)
           ]

    -- Apply state-specific attribute
    stateAttr :: Text -> AttrName
    stateAttr state = (attrName $ "jobState." <> toString state)

    -- Determine the job state widget with appropriate styling
    jobStateWidget =
      withAttr (stateAttr (jobState job)) (txt (jobState job))

  in
  border $
  padAll 1 $
  vBox
    [ hBox [ withAttr (attrName "jobLabel") (str "Job ID: ")
           , withAttr (attrName "jobValue") (txt (pack $ show (jobId job)))
           , withAttr (attrName "jobLabel") (str " State: ")
           , jobStateWidget
           ]
    , labeledField "Name" (name job)
    , labeledField "User" (userName job)
    , labeledField "Partition" (partition job)
    , labeledField "Nodes" (show (nodeCount job) <> " (" <> (nodes job) <> ")")
    , labeledField "CPUs" (show (cpus job))
    , labeledField "Memory/Node" (displayMaybeInt (memoryPerNode job) <> " MB")
    , labeledField "Time Limit" (displayTimeLimit (timeLimit job))
    , labeledField "Start Time" (formatUnixTime (startTime job))
    , labeledField "End Time" (formatUnixTime (endTime job))
    , labeledField "Exit Code" (pack $ show (exitCode job))
    , labeledField "State Reason" (stateReason job)
    ]


data TransientMsg = Cancel | Hold | Suspend | Resume | Hold | Release | Top | Nice | TimeLimit  deriving (Show, Eq)

-- Application state
data AppState = AppState
  { _searchEditor :: Editor Text Name
  , _jobList :: GenericList Name Vec.Vector Job
  , _allJobs :: [Job]
  , _selectedJob :: Maybe Job
  , _transient :: Maybe (TR.TransientState TransientMsg)
  } deriving (Show)

data SlewEvent = SQueueStatus [Job]

-- Widget names
data Name = SearchEditor | JobListWidget
  deriving (Eq, Ord, Show)


testTransient = TR.menu "Control" $ mconcat
  [ TR.item 'c' "Cancel" Cancel
  , TR.item 'h' "Hold" Hold
  , TR.item 's' "Suspend" Suspend
  , TR.item 'r' "Resume" Resume
  , TR.submenu 'p' "Job Priority" $ mconcat
      [ TR.item 't' "Set Top" Top
      , TR.item 'p' "Set Priority" Nice
      ]
  ]

text :: Text -> Widget n
text = str . T.unpack

-- Generate lenses
$(makeLenses ''AppState)

-- Initialize application state
initialState :: AppState
initialState = AppState
  { _searchEditor = editor SearchEditor (Just 1) ""
  , _jobList = list JobListWidget mempty 1
  , _allJobs = mempty
  , _selectedJob = Nothing
  , _transient = Nothing
  }

-- Filter jobs based on search term using lenses
filterJobs :: Text -> [Job] -> [Job]
filterJobs "" = id
filterJobs searchTerm = filter matchesSearch
  where
    searchLower = T.toLower searchTerm
    toLower = T.unpack . T.toLower . T.pack
    matchesSearch job = 
      let jobFields = [ show $ jobId job, name job, account job
                      , jobState job, partition job ]
      in any (searchLower `T.isInfixOf`) $ map T.toLower jobFields

-- Get current search term using lenses
getCurrentSearchTerm :: AppState -> Text
getCurrentSearchTerm = mconcat . getEditContents . view searchEditor

-- Get currently selected job using lenses
getSelectedJob :: AppState -> Maybe Job
getSelectedJob = fmap snd . listSelectedElement . view jobList

-- Update job list with filtered results
updateJobList :: Text -> AppState -> AppState
updateJobList searchTerm st =
  let filteredJobs = filterJobs searchTerm $ st ^. allJobs
      newJobList = listReplace (Vec.fromList filteredJobs) (Just 0) $ st ^. jobList
  in st & jobList .~ newJobList

-- Draw the application
drawApp :: AppState -> [Widget Name]
drawApp st = [ui]
  where
    ui = vBox
      [ drawSearchBar st
      , hBorder
      , drawJobList st
      , maybe (str "") drawJobPanel (st ^. selectedJob)
      , maybe (str "") TR.drawTransientView (st ^. transient)
      ]

-- Draw search bar using lenses
drawSearchBar :: AppState -> Widget Name
drawSearchBar st = str "Search Jobs: " <+> (renderEditor (text . T.unlines) True $ st ^. searchEditor)

-- Draw job list using lenses
drawJobList :: AppState -> Widget Name
drawJobList st =
  let searchTerm = getCurrentSearchTerm st
      filteredCount = length . filterJobs searchTerm $ st ^. allJobs
      currentList = st ^. jobList
  in borderWithLabel (str $ "SLURM Queue (" ++ show filteredCount ++ " jobs)") $ renderList drawJobItem True currentList
    

-- Format seconds to HH:MM:SS
formatTime :: Int -> String
formatTime seconds = 
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
      secs = seconds `mod` 60
  in printf "%d:%02d:%02d" hours minutes secs

-- Draw individual job item using lenses
drawJobItem :: Bool -> Job -> Widget Name
drawJobItem selected job =
  let style = if selected then withAttr (attrName "selected") else id
  in style . padRight Max . vBox $
    [ hBox
      [ padRight (Pad 2) . str . show $ jobId job
      , padRight (Pad 2) . text . T.take 12 $ name job
      , padRight (Pad 2) . text . T.take 8 $ account job
      , padRight (Pad 2) . text . T.take 10 $ jobState job 
      , padRight (Pad 2) . str . take 10 . formatTime $ timeLimit job
      , text $ nodes job
      ]
    ]


searchJobList :: EventM Name AppState ()
searchJobList = do
  st <- get
  put $ updateJobList (getCurrentSearchTerm st) st

-- Handle search editor events using zoom
handleSearchEvent :: V.Event -> EventM Name AppState ()
handleSearchEvent e = do
  zoom searchEditor $ handleEditorEvent (VtyEvent e)
  searchJobList

selectJob :: EventM Name AppState ()
selectJob = do
    selectedElement <- preuse (jobList . listSelectedElementL)
    selectedJob .= selectedElement

shell :: String -> BrickEvent Name AppState ()
shell cmd = do
  cmdHandle <- spawnCommand cmd
  void $ waitForProcess cmdHandle

handleTransientMsg :: TransientMsg -> BrickEvent Name AppState ()
handleTransientMsg Cancel = gets getSelectedJob >>= shell . cancelCmd
  where cancelCmd = fmt
-- Handle events with pattern matching at function level
handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent  (VtyEvent e@(V.EvKey V.KEsc [])) = do
  msg <- getFirst <$> zoom (transient . _Just) (handleTransientEvent e)
  case msg of
    Just TR.Close -> transient .= Nothing
    Just TR.Up -> pure ()
    _ -> halt
handleEvent  (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent e@(V.EvKey V.KUp [])) = zoom jobList (handleListEvent e) >> selectJob
handleEvent (VtyEvent e@(V.EvKey V.KDown [])) = zoom jobList (handleListEvent e) >> selectJob
handleEvent (VtyEvent e@(V.EvKey (V.KChar 'c') [V.MCtrl])) = transient .= Just testTransient
handleEvent (VtyEvent e) = do
  msg <- getFirst <$> zoom (transient . _Just) (handleTransientEvent e)
  case msg of
    Just TR.Close -> transient .= Nothing
    Just (TR.Msg m) -> transient .= Nothing >> handleTransientMsg m
    Just _ -> pure ()
    Nothing -> handleSearchEvent e >> selectJob

handleEvent e@(AppEvent (SQueueStatus sqJobs)) = do 
    jobList %= listReplace (fromList sqJobs) (Just 0)
    allJobs .= sqJobs
    searchJobList

-- Application attributes
appAttrs :: AttrMap
appAttrs = attrMap V.defAttr
  [ (attrName "selected", V.withStyle V.defAttr V.reverseVideo)
  , (attrName "highlight", fg V.cyan)
  , (attrName "running", fg V.green)
  , (attrName "completed", fg V.blue)
  , (attrName "pending", fg V.yellow)
  , (attrName "failed", fg V.red)
  , (attrName "jobState.PENDING", fg V.yellow)
  , (attrName "jobState.RUNNING", fg V.green)
  , (attrName "jobState.COMPLETED", fg V.blue)
  , (attrName "jobState.FAILED", fg V.red)
  , (attrName "jobState.CANCELLED", V.withStyle V.defAttr V.reverseVideo) -- Example, adjust as needed
  , (attrName "jobLabel", fg V.cyan)
  , (attrName "jobValue", fg V.white)
  ]

-- Brick application
app :: App AppState SlewEvent Name
app = App
  { appDraw = drawApp
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const appAttrs
  }

squeueThread :: BC.BChan SlewEvent -> IO ThreadId
squeueThread chan = forkIO $ do 
    forever $ do  
      squeueFile <- readCreateProcess (shell "squeue") ""
      case decode (fromString squeueFile) of
        Just sqStatus -> BC.writeBChan chan . SQueueStatus $ jobs sqStatus
        _ -> pure ()
      threadDelay 30_000_000


-- Main function
main :: IO ()
main = void $ do 
      eventChan <- BC.newBChan 10 
      let buildVty = mkVty defaultConfig
      initialVty <- buildVty
      squeueThread eventChan
      customMain initialVty buildVty (Just eventChan) app initialState
