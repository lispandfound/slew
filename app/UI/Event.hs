{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Event (
    handleEvent,
) where

import Brick
import Control.Lens hiding (zoom)
import qualified Graphics.Vty as V
import UI.SControl (SControlCmd (..), sendSControlCommand)

import Model.AppState
import Model.Job
import UI.JobList (handleJobQueueEvent, selectedJob, updateJobList, updateSortKey)
import UI.Poller (handlePollerEvent, tailFile)
import UI.SControl (logSControlEvent)
import qualified UI.Transient as TR

scontrolTransient :: TR.TransientState SlewEvent
scontrolTransient =
    TR.menu "Job Control" $
        mconcat
            [ TR.submenu 's' "State Control" $
                mconcat
                    [ TR.item 'h' "Hold" (SControlSend Hold)
                    , TR.item 'r' "Resume" (SControlSend Resume)
                    , TR.item 's' "Suspend" (SControlSend Suspend)
                    , TR.item 'c' "Cancel" (SControlSend Cancel)
                    ]
            , TR.submenu 'p' "Priority" $
                mconcat
                    [ TR.item 't' "Top" (SControlSend Top)
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
sortListByCat Account = comparing (view account)
sortListByCat CPUs = comparing (view cpus)
sortListByCat StartTime = comparing (view startTime)
sortListByCat EndTime = comparing (view endTime)
sortListByCat JobName = comparing (view name)
sortListByCat UserName = comparing (view userName)
sortListByCat Memory = comparing (view memoryPerNode)

-- | Main event handler
handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [V.MCtrl])) = showLog %= not
handleEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
    msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
    showingLog <- use showLog
    case (showingLog, msg) of
        (True, _) -> showLog .= False
        (False, Just TR.Close) -> transient .= Nothing
        _ -> halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
    transient .= Just scontrolTransient -- could parameterise this
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = do
    curFile <- use (jobQueueState . selectedJob . standardOutput)
    zoom pollState (tailFile curFile)
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) =
    transient .= Just sortTransient -- could parameterise this
handleEvent (VtyEvent e) = do
    msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
    case msg of
        Just TR.Close -> transient .= Nothing
        Just (TR.Msg msg') -> transient .= Nothing >> handleEvent (AppEvent msg')
        Just TR.Next -> pure ()
        _ -> zoom jobQueueState (handleJobQueueEvent e)
handleEvent (AppEvent (SQueueStatus jobs)) = zoom jobQueueState (updateJobList jobs)
handleEvent (AppEvent (SortBy category)) = zoom jobQueueState (updateSortKey (sortListByCat category))
handleEvent (AppEvent (SControlSend msg)) = do
    job <- preuse (jobQueueState . selectedJob)
    case job of
        Just job' -> do
            let cmd = scontrolCommand msg job'
            zoom scontrolLogState (sendSControlCommand cmd)
        Nothing -> pure ()
  where
    scontrolCommand Cancel job = CancelJob [job ^. jobId]
    scontrolCommand Suspend job = SuspendJob [job ^. jobId]
    scontrolCommand Resume job = ResumeJob [job ^. jobId]
    scontrolCommand Hold job = HoldJob [job ^. jobId]
    scontrolCommand Release job = ReleaseJob [job ^. jobId]
    scontrolCommand Top job = TopJob [job ^. jobId]
handleEvent (AppEvent (SControlReceive output)) = zoom scontrolLogState (logSControlEvent output)
handleEvent (AppEvent (PollEvent ev)) = zoom pollState (handlePollerEvent ev)
handleEvent _ = pure ()
