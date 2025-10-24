{-# LANGUAGE OverloadedStrings #-}

module UI.JobPanel (drawJobPanel) where

import Brick (
    Padding (Max),
    Widget,
    attrName,
    emptyWidget,
    hBox,
    padBottom,
    padLeftRight,
    str,
    txt,
    vBox,
    withAttr,
    (<=>),
 )
import Brick.Widgets.Border (border)
import qualified Data.Text as T
import Model.Job (
    ExitCode (..),
    Job (..),
    formatTime,
    showWith,
 )
import Optics.Operators ((^.))

import Control.Monad.Writer
import Data.Time.Clock.System (systemToUTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import UI.Themes (jobLabel, jobState)

-- | Render detailed job panel
drawJobPanel :: Job -> Widget n
drawJobPanel job =
    let labeledField :: Text -> Text -> Widget n
        labeledField label value =
            hBox
                [ withAttr jobLabel (txt (label <> ": "))
                , txt value
                ]

        stateAttr stateName = jobState <> attrName (toString stateName)
        jobStateWidget = foldr (\stateName widget -> withAttr (stateAttr $ stateName) (txt stateName) <=> widget) emptyWidget (job ^. #jobState)
     in border . padBottom Max . padLeftRight 1 . vBox . execWriter $ do
            tell
                [ hBox
                    [ withAttr jobLabel (str "Job ID: ")
                    , (txt . show $ job ^. #jobId)
                    , withAttr jobLabel (str " State: ")
                    , jobStateWidget
                    ]
                , labeledField "Name" (job ^. #name)
                , labeledField "User" (job ^. #userName)
                , labeledField "Partition" (job ^. #partition)
                , labeledField "Nodes" (job ^. #nodes)
                , labeledField "CPUs" (showWith show $ job ^. #cpus)
                , labeledField "Memory/Node" (showWith (\mem -> show mem <> " MB") (job ^. #memoryPerNode))
                , labeledField "Time Limit" (showWith formatTime $ job ^. #timeLimit)
                , labeledField "Start Time" (showWith (toText . iso8601Show . systemToUTCTime) $ job ^. #startTime)
                , labeledField "End Time" (showWith (toText . iso8601Show . systemToUTCTime) $ job ^. #endTime)
                ]
            when ("COMPLETED" `elem` (job ^. #jobState) || "FAILED" `elem` (job ^. #jobState)) $
                tell [labeledField "Exit Code" (T.intercalate " " $ job ^. #exitCode ^. #status)]
            when ((not . T.null) (job ^. #stateReason) && (job ^. #stateReason /= "None")) $
                tell [labeledField "State Reason" (job ^. #stateReason)]
