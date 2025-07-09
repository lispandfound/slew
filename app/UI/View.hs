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
    jobQueueState,
    pollState,
    scontrolLogState,
    showLog,
    transient,
 )
import UI.JobList (drawJobList, drawSearchBar, selectedJob)
import UI.JobPanel (drawJobPanel)
import UI.Poller (drawPoller)
import UI.SControl (drawSControlLog)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawApp :: AppState -> [Widget Name]
drawApp st =
    (if st ^. showLog then [drawSControlLog (st ^. scontrolLogState)] else [])
        <> [ vBox
                [ (drawSearchBar (st ^. jobQueueState) <=> hBorder)
                , (drawJobList (st ^. jobQueueState) <+> maybe emptyWidget drawJobPanel (st ^? jobQueueState . selectedJob))
                , drawPoller (st ^. pollState)
                , maybe emptyWidget drawTransientView (st ^. transient)
                ]
           ]
