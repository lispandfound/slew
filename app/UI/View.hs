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
import qualified Model.TimingState
import Model.ViewState (View (..), currentView)
import UI.Echo (drawEchoBuffer)
import UI.JobList (drawJobList, drawSearchBar, selectedJob)
import UI.JobPanel (drawJobPanel)
import UI.SlurmCommand (drawSlurmCommandLog)
import UI.Transient (drawTransientView)

-- | Top-level renderer for the entire application.
drawAppView :: View -> AppState -> [Widget Name]
drawAppView SQueueView st =
    let Model.TimingState.TimingState{currentTime = curTime, lastUpdate = lastUpd} = st ^. #timingState
     in [ vBox
            [ (drawSearchBar (st ^. #jobQueueState) <=> hBorder)
            , (drawJobList curTime lastUpd (st ^. #jobQueueState) <+> maybe emptyWidget drawJobPanel (selectedJob (st ^. #jobQueueState)))
            , maybe emptyWidget drawTransientView (st ^. #transient)
            ]
        ]
drawAppView CommandLogView st = [drawSlurmCommandLog (st ^. #scontrolLogState)]
drawAppView _ _ = [emptyWidget]

drawApp :: AppState -> [Widget Name]
drawApp st = [vBox (drawAppView (currentView (st ^. #viewState)) st <> [drawEchoBuffer (st ^. #echoState)])]
