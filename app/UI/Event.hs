module UI.Event (
    handleEvent,
) where

import Brick (BrickEvent (AppEvent, VtyEvent), EventM, halt, nestEventM, zoom)
import Brick.BChan (writeBChan)
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
import Optics.Getter (view)
import Optics.Operators ((^.))
import Optics.State (preuse, use)
import Optics.State.Operators ((%=), (.=))
import UI.JobList (handleJobQueueEvent, selectedJob, updateJobList, updateSortKey)
import UI.Poller (handlePollerEvent, tailFile)
import UI.SlurmCommand (SlurmCommandCmd (..), logSlurmCommandEvent, sendSlurmCommandCommand)
import UI.Transient (TransientMsg)
import qualified UI.Transient as TR

triggerSqueue :: EventM n AppState ()
triggerSqueue = do
    ch <- use #squeueChannel
    liftIO (writeBChan ch ())

scontrolTransient :: TR.TransientState SlewEvent
scontrolTransient =
    TR.menu "Job Control" $
        mconcat
            [ TR.submenu 's' "State Control" $
                mconcat
                    [ TR.item 'h' "Hold" (SlurmCommandSend Hold)
                    , TR.item 'r' "Resume" (SlurmCommandSend Resume)
                    , TR.item 's' "Suspend" (SlurmCommandSend Suspend)
                    , TR.item 'c' "Cancel" (SlurmCommandSend Cancel)
                    ]
            , TR.submenu 'p' "Priority" $
                mconcat
                    [ TR.item 't' "Top" (SlurmCommandSend Top)
                    ]
            ]

sortTransient :: TR.TransientState SlewEvent
sortTransient =
    TR.menu "Job Sorting" $
        mconcat
            [ TR.item 'a' "Account" (SortBy Account)
            , TR.item 'c' "CPUs" (SortBy CPUs)
            , TR.item 's' "Start Time" (SortBy StartTime)
            , TR.item 'e' "End Time" (SortBy EndTime)
            , TR.item 'j' "Job Name" (SortBy JobName)
            , TR.item 'u' "User Name" (SortBy UserName)
            , TR.item 'm' "Memory (per node)" (SortBy Memory)
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
handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = #showLog %= not
handleEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
    msg <- getFirst <$> (zoomTransient e)
    showingLog <- use #showLog

    case (showingLog, msg) of
        (True, _) -> #showLog .= False
        (False, Just TR.Close) -> #transient .= Nothing
        _ -> halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
    #transient .= Just scontrolTransient -- could parameterise this
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = do
    curJob <- selectedJob <$> use #jobQueueState
    case curJob of
        Just job -> zoom #pollState (tailFile (job ^. #standardOutput))
        Nothing -> pure ()
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
handleEvent (AppEvent (SlurmCommandReceive output)) = zoom #scontrolLogState (logSlurmCommandEvent output) >> triggerSqueue
handleEvent (AppEvent (PollEvent ev)) = zoom #pollState (handlePollerEvent ev)
handleEvent (AppEvent Tick) = do
    sysTime <- liftIO getSystemTime
    #currentTime .= sysTime
handleEvent _ = pure ()
