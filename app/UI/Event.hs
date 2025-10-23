module UI.Event (
    handleEventWithEcho,
) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, nestEventM, txt, zoom)
import Brick.BChan (writeBChan)
import Brick.Widgets.Core (withAttr)
import Data.Time.Clock.System (getSystemTime)
import qualified Graphics.Vty as V
import Model.AppState (
    AppState (..),
    Category (..),
    Command (Cancel, Hold, Release, Resume, Suspend, Top),
    Name,
    SlewEvent (..),
 )
import Model.Job (
    Job (..),
 )
import Optics.Core (Lens')
import Optics.Getter (view)
import Optics.Operators ((^.))
import Optics.State (preuse, use)
import Optics.State.Operators ((%=), (.=))
import UI.Echo (clear, echo)
import UI.JobList (handleJobQueueEvent, selectedJob, updateJobList, updateSortKey)
import UI.Poller (tailFile)
import UI.SlurmCommand (SlurmCommandCmd (..), SlurmCommandLogEntry (SlurmCommandLogEntry, result), logSlurmCommandEvent, sendSlurmCommandCommand)
import UI.Themes (header, transient)
import UI.Transient (TransientMsg)
import qualified UI.Transient as TR

triggerSqueue :: EventM n AppState ()
triggerSqueue = do
    ch <- use #squeueChannel
    liftIO (writeBChan ch ())

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

-- | Main event handler
handleEventWithEcho :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEventWithEcho e@(VtyEvent (V.EvKey _ _)) = zoom #echoState clear >> handleEvent e
handleEventWithEcho e = handleEvent e

handleJobFile :: Lens' Job FilePath -> EventM Name AppState ()
handleJobFile field = do
    mJob <- selectedJob <$> use #jobQueueState
    for_ mJob $ \job -> do
        result <- tailFile (job ^. field)
        either (zoom #echoState . echo) (const (pure ())) result

handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = #showLog %= not
handleEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
    msg <- getFirst <$> (zoomTransient e)
    showingLog <- use #showLog

    case (showingLog, msg) of
        (True, _) -> #showLog .= False
        (False, Just TR.Close) -> #transient .= Nothing
        _ -> halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
    #transient .= Just scontrolTransient -- could parameterise this
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = handleJobFile #standardOutput
handleEvent (VtyEvent (V.EvKey (V.KChar 'e') [V.MCtrl])) = handleJobFile #standardError
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [V.MCtrl])) = triggerSqueue
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) =
    #transient .= Just sortTransient -- could parameterise this
handleEvent (VtyEvent e) = do
    msg <- getFirst <$> (zoomTransient e)
    case msg of
        Just TR.Close -> #transient .= Nothing
        Just (TR.Msg msg') -> #transient .= Nothing >> handleEvent (AppEvent msg')
        Just TR.Next -> pure ()
        _ -> zoom #jobQueueState (handleJobQueueEvent e)
handleEvent (AppEvent (SQueueStatus jobs)) = zoom #jobQueueState (updateJobList jobs)
handleEvent (AppEvent (SortBy category)) = zoom #jobQueueState (updateSortKey (sortListByCat category))
handleEvent (AppEvent (SlurmCommandSend msg)) = do
    job <- fmap selectedJob <$> preuse #jobQueueState
    case join job of
        Just job' -> do
            let cmd = scontrolCommand msg job'
            zoom #scontrolLogState (sendSlurmCommandCommand cmd)
        Nothing -> pure ()
  where
    scontrolCommand :: Command -> Job -> SlurmCommandCmd
    scontrolCommand Cancel job = CancelJob [job ^. #jobId]
    scontrolCommand Suspend job = SuspendJob [job ^. #jobId]
    scontrolCommand Resume job = ResumeJob [job ^. #jobId]
    scontrolCommand Hold job = HoldJob [job ^. #jobId]
    scontrolCommand Release job = ReleaseJob [job ^. #jobId]
    scontrolCommand Top job = TopJob [job ^. #jobId]
handleEvent (AppEvent (SlurmCommandReceive output@(SlurmCommandLogEntry{result = Left _}))) = zoom #echoState (echo errorMessage) >> zoom #scontrolLogState (logSlurmCommandEvent output) >> triggerSqueue
  where
    errorMessage = "command failed, type C-l to see output"
handleEvent (AppEvent (SlurmCommandReceive output)) = zoom #scontrolLogState (logSlurmCommandEvent output) >> triggerSqueue
handleEvent (AppEvent Tick) = do
    sysTime <- liftIO getSystemTime
    #currentTime .= sysTime
handleEvent _ = pure ()
