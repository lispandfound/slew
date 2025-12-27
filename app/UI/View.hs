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
    View (..),
 )
import UI.Echo (drawEchoBuffer)
import UI.JobList (drawJobList, drawSearchBar, selectedJob)
import UI.JobPanel (drawJobPanel)
import UI.SlurmCommand (drawSlurmCommandLog)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawAppView :: View -> AppState -> [Widget Name]
drawAppView SQueueView st =
    [ vBox
        [ (drawSearchBar (st ^. #jobQueueState) <=> hBorder)
        , (drawJobList (st ^. #currentTime) (st ^. #lastUpdate) (st ^. #jobQueueState) <+> maybe emptyWidget drawJobPanel (selectedJob (st ^. #jobQueueState)))
        , maybe emptyWidget drawTransientView (st ^. #transient)
        ]
    ]
drawAppView CommandLogView st = [drawSlurmCommandLog (st ^. #scontrolLogState)]
drawAppView _ _ = [emptyWidget]

drawApp :: AppState -> [Widget Name]
drawApp st = [vBox (drawAppView (head (st ^. #view)) st <> [drawEchoBuffer (st ^. #echoState)])]
