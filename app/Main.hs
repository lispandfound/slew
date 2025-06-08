{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.Edit
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Text.Printf (printf)
import Control.Monad (void)
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Data.List (isInfixOf)
import qualified Data.Text as T

-- Job state enumeration
data JobState = Running | Pending | Completed | Failed | Other String
  deriving (Show, Eq, Ord)

-- Job information from squeue
data JobInfo = JobInfo
  { _jobId :: Int
  , _partition :: Text
  , _jobName :: Text
  , _user :: Text
  , _jobState :: JobState
  , _runTime :: Int  -- seconds
  , _nodes :: Int
  , _nodeList :: Text
  } deriving (Show, Eq)

-- Accounting information from sacct
data SacctEntry = SacctEntry
  { _sacctJobId :: Text
  , _sacctJobName :: Text
  , _sacctState :: Text
  , _sacctExitCode :: Text
  , _sacctSubmit :: Text
  , _sacctStart :: Text
  , _sacctEnd :: Text
  , _sacctElapsed :: Text
  , _sacctCPUTime :: Text
  , _sacctMaxRSS :: Text
  } deriving (Show)

-- Application state
data AppState = AppState
  { _searchEditor :: Editor Text Name
  , _jobList :: GenericList Name Vec.Vector JobInfo
  , _sacctInfo :: [SacctEntry]
  , _allJobs :: [JobInfo]
  } deriving (Show)

-- Widget names
data Name = SearchEditor | JobListWidget
  deriving (Eq, Ord, Show)

text :: Text -> Widget n
text = str . T.unpack

-- Generate lenses
$(makeLenses ''JobInfo)
$(makeLenses ''SacctEntry)
$(makeLenses ''AppState)

-- Initialize application state
initialState :: AppState
initialState = AppState
  { _searchEditor = editor SearchEditor (Just 1) ""
  , _jobList = list JobListWidget (Vec.fromList dummySqueueData) 1
  , _sacctInfo = dummySacctData
  , _allJobs = dummySqueueData
  }

-- Dummy squeue data
dummySqueueData :: [JobInfo]
dummySqueueData =
  [ JobInfo 12345 "gpu" "training_job" "alice" Running 8130 2 "node[01-02]"
  , JobInfo 12346 "cpu" "data_processing" "bob" Pending 0 1 "(Priority)"
  , JobInfo 12347 "gpu" "inference" "charlie" Running 2712 1 "node03"
  , JobInfo 12348 "cpu" "simulation" "alice" Completed 5445 4 "node[04-07]"
  , JobInfo 12349 "gpu" "model_train" "dave" Running 12138 2 "node[08-09]"
  , JobInfo 12350 "cpu" "analysis" "eve" Pending 0 2 "(Resources)"
  , JobInfo 12351 "gpu" "deep_learning" "frank" Running 930 1 "node10"
  ]

-- Dummy sacct data
dummySacctData :: [SacctEntry]
dummySacctData =
  [ SacctEntry "12345" "training_job" "RUNNING" "0:0" "2024-06-07T10:00:00" "2024-06-07T10:05:00" "Unknown" "2:15:30" "4:31:00" "2.5GB"
  , SacctEntry "12347" "inference" "RUNNING" "0:0" "2024-06-07T11:30:00" "2024-06-07T11:32:00" "Unknown" "0:45:12" "0:45:12" "1.2GB"
  , SacctEntry "12348" "simulation" "COMPLETED" "0:0" "2024-06-07T08:00:00" "2024-06-07T08:05:00" "2024-06-07T09:35:45" "1:30:45" "6:02:00" "8.1GB"
  , SacctEntry "12349" "model_train" "RUNNING" "0:0" "2024-06-07T09:15:00" "2024-06-07T09:20:00" "Unknown" "3:22:18" "6:44:36" "4.7GB"
  , SacctEntry "12351" "deep_learning" "RUNNING" "0:0" "2024-06-07T12:45:00" "2024-06-07T12:47:00" "Unknown" "0:15:30" "0:15:30" "3.2GB"
  ]

-- Filter jobs based on search term using lenses
filterJobs :: Text -> [JobInfo] -> [JobInfo]
filterJobs "" = id
filterJobs searchTerm = filter matchesSearch
  where
    searchLower = T.toLower searchTerm
    toLower = T.unpack . T.toLower . T.pack
    matchesSearch job = 
      let jobFields = [ show $ job ^. jobId, job ^. jobName, job ^. user
                      , show $ job ^. jobState, job ^. partition ]
      in any (searchLower `T.isInfixOf`) $ map T.toLower jobFields

-- Get current search term using lenses
getCurrentSearchTerm :: AppState -> Text
getCurrentSearchTerm = mconcat . getEditContents . view searchEditor

-- Get currently selected job using lenses
getSelectedJob :: AppState -> Maybe JobInfo
getSelectedJob = fmap snd . listSelectedElement . view jobList

-- Get relevant sacct entries for selected job
getRelevantSacctEntries :: AppState -> Maybe SacctEntry
getRelevantSacctEntries st = 
  case getSelectedJob st of
    Nothing -> Nothing
    Just job -> find ((== show (job ^. jobId)) . view sacctJobId) $ st ^. sacctInfo

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
      , hBox [drawJobList st, vBorder, drawSacctPanel st]
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
  in vBox
    [ borderWithLabel (str $ "SLURM Queue (" ++ show filteredCount ++ " jobs)") $
      renderList drawJobItem True currentList
    , drawJobListHelp
    ]

-- Format seconds to HH:MM:SS
formatTime :: Int -> String
formatTime seconds = 
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
      secs = seconds `mod` 60
  in printf "%d:%02d:%02d" hours minutes secs

-- Draw individual job item using lenses
drawJobItem :: Bool -> JobInfo -> Widget Name
drawJobItem selected job =
  let style = if selected then withAttr (attrName "selected") else id
  in style . padRight Max . vBox $
    [ hBox
      [ padRight (Pad 2) . str . show $ job ^. jobId
      , padRight (Pad 2) . text . T.take 12 $ job ^. jobName
      , padRight (Pad 2) . text . T.take 8 $ job ^. user
      , padRight (Pad 2) . str . take 10 . show $ job ^. jobState
      , padRight (Pad 2) . str . take 10 . formatTime $ job ^. runTime
      , text $ job ^. nodeList
      ]
    ]

-- Draw help text for job list
drawJobListHelp :: Widget Name
drawJobListHelp = 
  borderWithLabel (str "Controls") . padAll 1 $ vBox
    [ str "↑/↓: Navigate jobs"
    , str "Tab: Switch to search"
    , str "q: Quit"
    ]

-- Draw sacct panel using lenses
drawSacctPanel :: AppState -> Widget Name
drawSacctPanel = borderWithLabel (str "Job Accounting (sacct)") . hLimit 60 . maybe (str "No accounting data available") drawSacctEntry . getRelevantSacctEntries

-- Draw individual sacct entry using lenses
drawSacctEntry :: SacctEntry -> Widget Name
drawSacctEntry entry = vBox
  [ hBox [str "Job ID: ", text $ entry ^. sacctJobId]
  , hBox [str "Name: ", text $ entry ^. sacctJobName]
  , hBox [str "State: ", text $ entry ^. sacctState]
  , hBox [str "Submit: ", text . T.take 16 $ entry ^. sacctSubmit]
  , hBox [str "Start: ", text . T.take 16 $ entry ^. sacctStart]
  , hBox [str "End: ", text . T.take 16 $ entry ^. sacctEnd]
  , hBox [str "Elapsed: ", text $ entry ^. sacctElapsed]
  , hBox [str "CPU Time: ", text $ entry ^. sacctCPUTime]
  , hBox [str "Max RSS: ", text $ entry ^. sacctMaxRSS]
  , str " "
  ]


-- Get attribute name based on job state
stateAttr :: JobState -> AttrName
stateAttr = \case
  Running -> attrName "running"
  Completed -> attrName "completed"
  Pending -> attrName "pending"
  Failed -> attrName "failed"
  _ -> attrName "default"

-- Handle search editor events using zoom
handleSearchEvent :: V.Event -> EventM Name AppState ()
handleSearchEvent e = do
  zoom searchEditor $ handleEditorEvent (VtyEvent e)
  st <- get
  put $ updateJobList (getCurrentSearchTerm st) st


-- Handle events with pattern matching at function level
handleEvent :: BrickEvent Name e -> EventM Name AppState ()
handleEvent  (VtyEvent (V.EvKey V.KEsc [])) = halt 
handleEvent  (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt 
handleEvent (VtyEvent e@(V.EvKey V.KUp [])) = zoom jobList (handleListEvent e)
handleEvent (VtyEvent e@(V.EvKey V.KDown [])) = zoom jobList (handleListEvent e)
handleEvent (VtyEvent e) = handleSearchEvent e >> zoom jobList (handleListEvent e)

-- Application attributes
appAttrs :: AttrMap
appAttrs = attrMap V.defAttr
  [ (attrName "selected", V.withStyle V.defAttr V.reverseVideo)
  , (attrName "highlight", fg V.cyan)
  , (attrName "running", fg V.green)
  , (attrName "completed", fg V.blue)
  , (attrName "pending", fg V.yellow)
  , (attrName "failed", fg V.red)
  ]

-- Brick application
app :: App AppState e Name
app = App
  { appDraw = drawApp
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const appAttrs
  }

-- Main function
main :: IO ()
main = void $ defaultMain app initialState
