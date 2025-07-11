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
import Optics.Operators ((^.))

import Model.AppState (
    AppState (..),
    Name (..),
 )
import UI.Echo (drawEchoBuffer)
import UI.JobList (drawJobList, drawSearchBar, selectedJob)
import UI.JobPanel (drawJobPanel)
import UI.Poller (drawPoller)
import UI.SlurmCommand (drawSlurmCommandLog)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawApp :: AppState -> [Widget Name]
drawApp st =
    (if st ^. #showLog then [drawSlurmCommandLog (st ^. #scontrolLogState)] else [])
        <> [ vBox
                [ (drawSearchBar (st ^. #jobQueueState) <=> hBorder)
                , (drawJobList (st ^. #currentTime) (st ^. #jobQueueState) <+> maybe emptyWidget drawJobPanel (selectedJob (st ^. #jobQueueState)))
                , drawPoller (st ^. #pollState)
                , maybe emptyWidget drawTransientView (st ^. #transient)
                , drawEchoBuffer (st ^. #echoState)
                ]
           ]
