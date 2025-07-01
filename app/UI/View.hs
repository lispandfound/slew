{-# LANGUAGE OverloadedStrings #-}

module UI.View
  ( drawApp
  , drawJobPanel
  , drawSearchBar
  , drawJobList
  , drawJobItem
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Edit
import Brick.Widgets.List
import Text.Printf (printf)
import Control.Lens
import qualified Data.Text as T

import Model.AppState
import Model.Job
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawApp :: AppState -> [Widget Name]
drawApp st = [ui]
  where
    ui = vBox
      [ drawSearchBar st
      , hBorder
      , drawJobList st
      , maybe (str "") drawJobPanel (st ^. selectedJob)
      , maybe (str "") drawTransientView (st ^. transient)
      ]

-- | Render the search bar.
drawSearchBar :: AppState -> Widget Name
drawSearchBar st =
  str "Search Jobs: " <+> renderEditor (txt . T.unlines) True (st ^. searchEditor)

-- | Render the job list widget.
drawJobList :: AppState -> Widget Name
drawJobList st =
  let searchTerm = getCurrentSearchTerm st
      filteredCount = length . filterJobs searchTerm $ st ^. allJobs
      currentList = st ^. jobList
  in borderWithLabel (str $ "SLURM Queue (" ++ show filteredCount ++ " jobs)") $
     renderList drawJobItem True currentList

-- | Render a single item in the job list.
drawJobItem :: Bool -> Job -> Widget Name
drawJobItem selected job =
  let style = if selected then withAttr (attrName "selected") else id
  in style . padRight Max . vBox $
    [ hBox
      [ padRight (Pad 2) . str . show $ jobId job
      , padRight (Pad 2) . txt . T.take 12 $ name job
      , padRight (Pad 2) . txt . T.take 8 $ account job
      , padRight (Pad 2) . txt . T.take 10 $ jobState job
      , padRight (Pad 2) . str . take 10 . formatTime $ timeLimit job
      , txt $ nodes job
      ]
    ]

-- | Format seconds to HH:MM:SS string
formatTime :: Int -> String
formatTime seconds =
  let hours = seconds `div` 3600
      minutes = (seconds `mod` 3600) `div` 60
      secs = seconds `mod` 60
  in printf "%d:%02d:%02d" hours minutes secs

-- | Render detailed job panel
drawJobPanel :: Job -> Widget Name
drawJobPanel job =
  let labeledField :: Text -> Text -> Widget Name
      labeledField label value =
        hBox [ withAttr (attrName "jobLabel") (txt (label <> ": "))
             , withAttr (attrName "jobValue") (txt value)
             ]

      stateAttr state = attrName $ "jobState." <> T.unpack state
      jobStateWidget = withAttr (stateAttr $ jobState job) (txt $ jobState job)

      displayMaybeInt = fromMaybe "N/A" . fmap show
      displayTimeLimit t = if t == 0 then "UNLIMITED" else show t <> " min"
      formatUnixTime = show -- TODO: Move this to a real formatter in Utils
  in border . padAll 1 . vBox $
     [ hBox [ withAttr (attrName "jobLabel") (str "Job ID: ")
            , withAttr (attrName "jobValue") (txt . show  $ jobId job)
            , withAttr (attrName "jobLabel") (str " State: ")
            , jobStateWidget
            ]
     , labeledField "Name" (name job)
     , labeledField "User" (userName job)
     , labeledField "Partition" (partition job)
     , labeledField "Nodes" ((show (nodeCount job)) <> " (" <> nodes job <> ")")
     , labeledField "CPUs" (show $ cpus job)
     , labeledField "Memory/Node" (displayMaybeInt (memoryPerNode job) <> " MB")
     , labeledField "Time Limit" (displayTimeLimit $ timeLimit job)
     , labeledField "Start Time" (formatUnixTime $ startTime job)
     , labeledField "End Time" (formatUnixTime $ endTime job)
     , labeledField "Exit Code" (show $ exitCode job)
     , labeledField "State Reason" (stateReason job)
     ]
