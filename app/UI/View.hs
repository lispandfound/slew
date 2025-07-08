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
    pollState,
    selectedJob,
    transient,
 )
import UI.JobList (drawJobList, drawSearchBar)
import UI.JobPanel (drawJobPanel)
import UI.Poller (drawPoller)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawApp :: AppState -> [Widget Name]
drawApp st =
    [ vBox
        [ (drawSearchBar st <=> hBorder)
        , (drawJobList st <+> maybe emptyWidget drawJobPanel (st ^? selectedJob))
        , drawPoller (st ^. pollState)
        , maybe emptyWidget drawTransientView (st ^. transient)
        ]
    ]
