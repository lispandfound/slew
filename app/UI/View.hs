{-# LANGUAGE OverloadedStrings #-}

module UI.View (
    drawApp,
    drawSearchBar,
    drawJobList,
) where

import Brick (
    Widget,
    emptyWidget,
    vBox,
    (<+>),
    (<=>),
 )
import Brick.Widgets.Border (hBorder)
import Control.Lens

import Model.AppState (
    AppState,
    Name,
    currentTime,
    jobQueueState,
    pollState,
    scontrolLogState,
    showLog,
    transient,
 )
import UI.JobList (drawJobList, drawSearchBar, selectedJob)
import UI.JobPanel (drawJobPanel)
import UI.Poller (drawPoller)
import UI.SlurmCommand (drawSlurmCommandLog)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawApp :: AppState -> [Widget Name]
drawApp st =
    (if st ^. showLog then [drawSlurmCommandLog (st ^. scontrolLogState)] else [])
        <> [ vBox
                [ (drawSearchBar (st ^. jobQueueState) <=> hBorder)
                , (drawJobList (st ^. currentTime) (st ^. jobQueueState) <+> maybe emptyWidget drawJobPanel (st ^? jobQueueState . selectedJob))
                , drawPoller (st ^. pollState)
                , maybe emptyWidget drawTransientView (st ^. transient)
                ]
           ]
