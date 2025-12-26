module UI.Event (
    handleEvent,
) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, nestEventM, txt, zoom)
import Brick.Widgets.Core (withAttr)
import Data.List.NonEmpty ((<|))
import Data.Time.Clock.System (getSystemTime)
import qualified Graphics.Vty as V
import Model.AppState (
    AppState (..),
    Category (..),
    Name,
    SlewEvent (..),
    View (..),
 )
import Model.Job (
    Job (..),
 )
import Model.Options (Options (tailTemplate))
import Model.SQueue (SlurmResponse (..))
import Model.SlurmCommand (Command (Cancel, Hold, Resume, Suspend, Top), SlurmCommandResult (..), slurm, squeue)
import Optics.Core (Lens')
import Optics.Getter (view)
import Optics.Operators ((^.))
import Optics.State (preuse, use)
import Optics.State.Operators ((%=), (.=))
import Slurm.Channel (runDiscard, runJsonErr)
import UI.Echo (clear, echo)
import UI.JobList (handleJobQueueEvent, selectedJob, updateJobList, updateSortKey)
import UI.Poller (tailFile)
import UI.SlurmCommand (logSlurmCommandEvent)
import UI.Themes (header, transient)
import UI.Transient (TransientMsg)
import qualified UI.Transient as TR

triggerSqueue :: EventM n AppState ()
triggerSqueue = do
    ch <- use #worker
    liftIO (runJsonErr ch squeue SQueueStatus)

bumpUpdateTime :: EventM n AppState ()
bumpUpdateTime = do
    curTime <- liftIO getSystemTime
    #lastUpdate .= pure curTime

scontrolTransient :: TR.TransientState SlewEvent Name
scontrolTransient =
    TR.menu "Job Control" $
        TR.horizontalLayout $
            [ TR.submenu 's' "State Control" $
                TR.horizontalLayout
                    [ TR.verticalLayoutWithLabel
                        (withAttr (transient <> header) $ txt "Stop or Start")
                        [ TR.item 'h' "Hold" (SlurmCommandSend Hold)
                        , TR.item 'r' "Resume" (SlurmCommandSend Resume)
                        , TR.item 's' "Suspend" (SlurmCommandSend Suspend)
                        , TR.item 'c' "Cancel" (SlurmCommandSend Cancel)
                        ]
                    , TR.verticalLayoutWithLabel
                        (withAttr (transient <> header) $ txt "Priority")
                        [ TR.item 't' "Top" (SlurmCommandSend Top)
                        ]
                    ]
            ]

sortTransient :: TR.TransientState SlewEvent Name
sortTransient =
    TR.menu "Job Sorting" $
        TR.horizontalLayout
            [ TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Name of")
                [ TR.item
                    'a'
                    "Account"
                    (SortBy Account)
                , TR.item
                    'u'
                    "User"
                    (SortBy UserName)
                , TR.item 'j' "Job" (SortBy JobName)
                ]
            , TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Time")
                [ TR.item 's' "Start Time" (SortBy StartTime)
                , TR.item 'e' "End Time" (SortBy EndTime)
                ]
            , TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Resources")
                [ TR.item 'c' "CPUs" (SortBy CPUs)
                , TR.item 'm' "Memory (per node)" (SortBy Memory)
                ]
            ]

sortListByCat :: Category -> Job -> Job -> Ordering
sortListByCat Account = comparing (view #account)
sortListByCat CPUs = comparing (view #cpus)
sortListByCat StartTime = comparing (view #startTime)
sortListByCat EndTime = comparing (view #endTime)
sortListByCat JobName = comparing (view #name)
sortListByCat UserName = comparing (view #userName)
sortListByCat Memory = comparing (view #memoryPerNode)

zoomTransient :: V.Event -> EventM Name AppState (First (TransientMsg SlewEvent))
zoomTransient e = do
    trMay <- use #transient
    case trMay of
        Just tr -> do
            (tr', msg) <- nestEventM tr (TR.handleTransientEvent e)
            #transient .= Just tr'
            pure msg
        Nothing -> pure mempty

handleJobFile :: Lens' Job FilePath -> EventM Name AppState ()
handleJobFile field = do
    mJob <- selectedJob <$> use #jobQueueState
    for_ mJob $ \job -> do
        opts <- use #options
        result <- tailFile (job ^. field) (opts ^. #tailTemplate)
        either (zoom #echoState . echo) (const (pure ())) result

handleSQueueViewEvent :: BrickEvent Name SlewEvent -> EventM Name AppState Bool
handleSQueueViewEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
    msg <- getFirst <$> zoomTransient e
    case msg of
        Just TR.Close -> #transient .= Nothing >> pure True
        Just TR.Up -> pure True
        _ -> pure False
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
    #transient .= Just scontrolTransient >> pure True -- could parameterise this
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = handleJobFile #standardOutput >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = handleJobFile #standardError >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = triggerSqueue >> pure True
handleSQueueViewEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) =
    #transient .= Just sortTransient >> pure True -- could parameterise this
handleSQueueViewEvent (VtyEvent e) = do
    msg <- getFirst <$> zoomTransient e
    case msg of
        Just TR.Close -> #transient .= Nothing >> pure True
        Just (TR.Msg msg') -> #transient .= Nothing >> handleSQueueViewEvent (AppEvent msg')
        Just TR.Next -> pure True
        _ -> zoom #jobQueueState (handleJobQueueEvent e)
handleSQueueViewEvent (AppEvent (SortBy category)) = zoom #jobQueueState (updateSortKey (sortListByCat category)) >> pure True
handleSQueueViewEvent (AppEvent (SlurmCommandSend msg)) = do
    job <- fmap selectedJob <$> preuse #jobQueueState
    case join job of
        Just job' -> do
            ch <- use #worker
            liftIO $ runDiscard ch (slurm msg (job' ^. #jobId)) SlurmCommandReceive
            return True
        Nothing -> pure False
handleSQueueViewEvent _ = pure False

handleCommandLogViewEvent :: BrickEvent Name SlewEvent -> EventM Name AppState Bool
handleCommandLogViewEvent = const (pure False)

handleNodeViewEvent :: BrickEvent Name SlewEvent -> EventM Name AppState Bool
handleNodeViewEvent = const (pure False)

isKeyPress :: BrickEvent Name SlewEvent -> Bool
isKeyPress (VtyEvent (V.EvKey _ _)) = True
isKeyPress _ = False

pushView :: View -> EventM Name AppState ()
pushView newView = #view %= (newView <|)

popView :: EventM Name AppState ()
popView = #view %= tail'
  where
    -- tail' is a tail that does not change singletons to preserve the NonEmpty invariant.
    tail' (x :| []) = x :| []
    tail' (_ :| (y : ys)) = y :| ys

haltIfSingleton :: EventM Name AppState ()
haltIfSingleton = do
    viewStack <- use #view
    when ((null . tail) viewStack) halt

handleGlobalEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleGlobalEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
handleGlobalEvent (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = pushView CommandLogView
handleGlobalEvent (VtyEvent (V.EvKey V.KEsc [])) = haltIfSingleton >> popView
handleGlobalEvent _ = pure ()

handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (AppEvent (SlurmCommandReceive output@(SlurmCommandResult{result = Left _}))) = zoom #echoState (echo errorMessage) >> zoom #scontrolLogState (logSlurmCommandEvent output) >> triggerSqueue
  where
    errorMessage = "command failed, type C-l to see output"
handleEvent (AppEvent (SlurmCommandReceive output)) = zoom #scontrolLogState (logSlurmCommandEvent output) >> triggerSqueue
handleEvent (AppEvent Tick) = do
    sysTime <- liftIO getSystemTime
    #currentTime .= sysTime
handleEvent (AppEvent (SQueueStatus output@(SlurmCommandResult{result = Left _}))) =
    zoom #echoState (echo errorMessage)
        >> zoom
            #scontrolLogState
            (logSlurmCommandEvent (void output))
        >> bumpUpdateTime
  where
    errorMessage = "squeue --json failed, type C-l to see output"
handleEvent (AppEvent (SQueueStatus (SlurmCommandResult{result = Right (SlurmResponse{jobs = jobs})}))) = zoom #jobQueueState (updateJobList jobs) >> bumpUpdateTime
handleEvent e = do
    curView <- use #view
    handled <- case head curView of
        SQueueView -> handleSQueueViewEvent e
        CommandLogView -> handleCommandLogViewEvent e
        NodeView -> handleNodeViewEvent e
    unless handled (handleGlobalEvent e)
    when (isKeyPress e) (zoom #echoState clear)
