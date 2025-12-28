module UI.Event (
    handleEvent,
) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, nestEventM, zoom)
import qualified Graphics.Vty as V
import Model.AppState (
    AppState (..),
    Category (..),
    Name,
    SlewEvent (..),
 )
import Model.Job (
    Job (..),
 )
import Model.Options (Options (tailTemplate))
import Model.SQueue (SlurmResponse (..))
import Model.SlurmCommand (Command, SlurmCommandResult (..), slurm, squeue)
import Model.ViewState (View (..), currentView)
import UI.TimingState (handleTick, handleUpdateTime)
import UI.ViewState (handleViewPopWithHalt, handleViewPush)
import Optics.Core (Lens')
import Optics.Operators ((^.))
import Optics.State (preuse, use)
import Optics.State.Operators ((.=))
import Slurm.Channel (runDiscard, runJsonErr)
import UI.Echo (clear, echo)
import UI.JobList (handleJobQueueEvent, selectedJob, updateJobList, updateSortKey)
import UI.Menus (scontrolTransient, sortListByCat, sortTransient)
import UI.Poller (tailFile)
import UI.SlurmCommand (logSlurmCommandEvent)
import UI.Transient (TransientMsg)
import qualified UI.Transient as TR

------------------------------------------------------------
-- SQueue Operations

triggerSqueue :: EventM n AppState ()
triggerSqueue = do
    ch <- use #worker
    liftIO (runJsonErr ch squeue SQueueStatus)

bumpUpdateTime :: EventM n AppState ()
bumpUpdateTime = zoom #timingState handleUpdateTime

------------------------------------------------------------
-- Transient Menu Handling

zoomTransient :: V.Event -> EventM Name AppState (First (TransientMsg SlewEvent))
zoomTransient e = do
    trMay <- use #transient
    case trMay of
        Just tr -> do
            (tr', msg) <- nestEventM tr (TR.handleTransientEvent e)
            #transient .= Just tr'
            pure msg
        Nothing -> pure mempty

closeTransient :: EventM Name AppState ()
closeTransient = #transient .= Nothing

openTransient :: TR.TransientState SlewEvent Name -> EventM Name AppState ()
openTransient menu = #transient .= Just menu

------------------------------------------------------------
-- Job File Operations

handleJobFile :: Lens' Job FilePath -> EventM Name AppState ()
handleJobFile field = do
    mJob <- selectedJob <$> use #jobQueueState
    for_ mJob $ \job -> do
        opts <- use #options
        result <- tailFile (job ^. field) (opts ^. #tailTemplate)
        either (zoom #echoState . echo) (const (pure ())) result

------------------------------------------------------------
-- SQueue View Event Handlers

handleSQueueViewEvent :: BrickEvent Name SlewEvent -> EventM Name AppState Bool
handleSQueueViewEvent (VtyEvent e@(V.EvKey V.KEsc [])) =
    handleTransientEscape e
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
    openTransient scontrolTransient >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) =
    handleJobFile #standardOutput >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) =
    handleJobFile #standardError >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) =
    triggerSqueue >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) =
    openTransient sortTransient >> pure True
handleSQueueViewEvent (VtyEvent e) =
    handleTransientOrJobList e
handleSQueueViewEvent (AppEvent (SortBy category)) =
    zoom #jobQueueState (updateSortKey (sortListByCat category)) >> pure True
handleSQueueViewEvent (AppEvent (SlurmCommandSend msg)) =
    handleSlurmCommand msg
handleSQueueViewEvent _ = pure False

handleTransientEscape :: V.Event -> EventM Name AppState Bool
handleTransientEscape e = do
    msg <- getFirst <$> zoomTransient e
    case msg of
        Just TR.Close -> closeTransient >> pure True
        Just TR.Up -> pure True
        _ -> pure False

handleTransientOrJobList :: V.Event -> EventM Name AppState Bool
handleTransientOrJobList e = do
    msg <- getFirst <$> zoomTransient e
    case msg of
        Just TR.Close -> closeTransient >> pure True
        Just (TR.Msg msg') -> closeTransient >> handleSQueueViewEvent (AppEvent msg')
        Just TR.Next -> pure True
        _ -> zoom #jobQueueState (handleJobQueueEvent e)

handleSlurmCommand :: Command -> EventM Name AppState Bool
handleSlurmCommand msg = do
    job <- fmap selectedJob <$> preuse #jobQueueState
    case join job of
        Just job' -> do
            ch <- use #worker
            liftIO $ runDiscard ch (slurm msg (job' ^. #jobId)) SlurmCommandReceive
            return True
        Nothing -> pure False

------------------------------------------------------------
-- Other View Event Handlers

handleCommandLogViewEvent :: BrickEvent Name SlewEvent -> EventM Name AppState Bool
handleCommandLogViewEvent = const (pure False)

handleNodeViewEvent :: BrickEvent Name SlewEvent -> EventM Name AppState Bool
handleNodeViewEvent = const (pure False)

------------------------------------------------------------
-- View Management

isKeyPress :: BrickEvent Name SlewEvent -> Bool
isKeyPress (VtyEvent (V.EvKey _ _)) = True
isKeyPress _ = False

------------------------------------------------------------
-- Global Event Handlers

handleGlobalEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleGlobalEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
handleGlobalEvent (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = zoom #viewState (handleViewPush CommandLogView)
handleGlobalEvent (VtyEvent (V.EvKey V.KEsc [])) = zoom #viewState handleViewPopWithHalt
handleGlobalEvent _ = pure ()

------------------------------------------------------------
-- App Event Handlers

handleSlurmCommandReceive :: SlurmCommandResult () -> EventM Name AppState ()
handleSlurmCommandReceive output@(SlurmCommandResult{result = Left _}) =
    zoom #echoState (echo errorMessage)
        >> zoom #scontrolLogState (logSlurmCommandEvent output)
        >> triggerSqueue
  where
    errorMessage = "command failed, type C-l to see output"
handleSlurmCommandReceive output =
    zoom #scontrolLogState (logSlurmCommandEvent output) >> triggerSqueue

-- handleTick is now imported from UI.TimingState and used with zoom
handleAppTick :: EventM Name AppState ()
handleAppTick = zoom #timingState handleTick

handleSQueueStatus :: SlurmCommandResult SlurmResponse -> EventM Name AppState ()
handleSQueueStatus output@(SlurmCommandResult{result = Left _}) =
    zoom #echoState (echo errorMessage)
        >> zoom #scontrolLogState (logSlurmCommandEvent (void output))
        >> bumpUpdateTime
  where
    errorMessage = "squeue --json failed, type C-l to see output"
handleSQueueStatus (SlurmCommandResult{result = Right (SlurmResponse{jobs = jobs})}) =
    zoom #jobQueueState (updateJobList jobs) >> bumpUpdateTime

------------------------------------------------------------
-- Main Event Handler

handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (AppEvent (SlurmCommandReceive output)) =
    handleSlurmCommandReceive output
handleEvent (AppEvent Tick) =
    handleAppTick
handleEvent (AppEvent (SQueueStatus output)) =
    handleSQueueStatus output
handleEvent e = do
    vs <- use #viewState
    handled <- case currentView vs of
        SQueueView -> handleSQueueViewEvent e
        CommandLogView -> handleCommandLogViewEvent e
        NodeView -> handleNodeViewEvent e
    unless handled (handleGlobalEvent e)
    when (isKeyPress e) (zoom #echoState clear)
