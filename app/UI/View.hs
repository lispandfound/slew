{-# LANGUAGE OverloadedStrings #-}

module UI.View (
    drawApp,
    drawJobPanel,
    drawSearchBar,
    drawJobList,
    drawJobItem,
) where

import Brick (
    Padding (Max, Pad),
    Widget,
    attrName,
    emptyWidget,
    hBox,
    padBottom,
    padLeftRight,
    padRight,
    str,
    txt,
    vBox,
    withAttr,
    (<+>),
    (<=>),
 )
import Brick.Widgets.Border (border, borderWithLabel, hBorder)
import Brick.Widgets.Edit (renderEditor)
import Brick.Widgets.List (renderList)
import Control.Lens
import qualified Data.Text as T
import Data.Time.Clock (DiffTime)

import Data.Time.Clock.System (systemToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Fmt
import Model.AppState (
    AppState,
    Name,
    allJobs,
    filterJobs,
    getCurrentSearchTerm,
    jobList,
    pollState,
    pollTitle,
    searchEditor,
    selectedJob,
    transient,
 )
import Model.Job (
    ExitCode (status),
    Job (
        account,
        cpus,
        endTime,
        exitCode,
        jobId,
        jobState,
        memoryPerNode,
        name,
        nodes,
        partition,
        startTime,
        stateReason,
        timeLimit,
        userName
    ),
    showWith,
 )
import UI.Poller (buffer, renderPoller)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawApp :: AppState -> [Widget Name]
drawApp st =
    [ vBox
        [ (drawSearchBar st <=> hBorder)
        , (drawJobList st <+> maybe emptyWidget drawJobPanel (st ^. selectedJob))
        , drawStdOutTailer st
        , maybe emptyWidget drawTransientView (st ^. transient)
        ]
    ]

drawStdOutTailer :: AppState -> Widget Name
drawStdOutTailer st = if (null $ st ^. pollState ^. buffer) then emptyWidget else borderWithLabel (txt . fromMaybe "Output" $ st ^. pollTitle) . padRight Max $ renderPoller (st ^. pollState)

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
                , padRight (Pad 2) . txt . T.take 10 . T.intercalate " " $ jobState job
                , padRight (Pad 2) . txt . T.take 10 . showWith formatTime $ timeLimit job
                , txt $ nodes job
                ]
            ]

-- | Format seconds to HH:MM:SS string
formatTime :: DiffTime -> Text
formatTime diff =
    let
        seconds = round diff :: Int
        hours = seconds `div` 3600
        minutes = (seconds `mod` 3600) `div` 60
        secs = seconds `mod` 60
     in
        "" +| padLeftF 2 '0' hours |+ ":" +| padLeftF 2 '0' minutes |+ ":" +| padLeftF 2 '0' secs |+ ""

-- | Render detailed job panel
drawJobPanel :: Job -> Widget Name
drawJobPanel job =
    let labeledField :: Text -> Text -> Widget Name
        labeledField label value =
            hBox
                [ withAttr (attrName "jobLabel") (txt (label <> ": "))
                , withAttr (attrName "jobValue") (txt value)
                ]

        stateAttr stateName = attrName $ "jobState." <> T.unpack stateName
        jobStateWidget = foldr (\stateName widget -> withAttr (stateAttr $ stateName) (txt stateName) <=> widget) emptyWidget (jobState job)
     in border . padBottom Max . padLeftRight 1 . vBox $
            [ hBox
                [ withAttr (attrName "jobLabel") (str "Job ID: ")
                , withAttr (attrName "jobValue") (txt . show $ jobId job)
                , withAttr (attrName "jobLabel") (str " State: ")
                , jobStateWidget
                ]
            , labeledField "Name" (name job)
            , labeledField "User" (userName job)
            , labeledField "Partition" (partition job)
            , labeledField "Nodes" (nodes job)
            , labeledField "CPUs" (showWith show $ cpus job)
            , labeledField "Memory/Node" (showWith (\mem -> show mem <> " MB") (memoryPerNode job))
            , labeledField "Time Limit" (showWith formatTime $ timeLimit job)
            , labeledField "Start Time" (showWith (T.pack . iso8601Show . systemToUTCTime) $ startTime job)
            , labeledField "End Time" (showWith (T.pack . iso8601Show . systemToUTCTime) $ endTime job)
            , labeledField "Exit Code" (T.intercalate " " . status $ exitCode job)
            , labeledField "State Reason" (stateReason job)
            ]
