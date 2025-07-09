{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Event (
    handleEvent,
    shellWithJob,
) where

import Brick
import Control.Lens hiding (zoom)
import Fmt
import qualified Graphics.Vty as V
import System.Process (spawnCommand, waitForProcess)

import Model.AppState
import Model.Job
import UI.JobList (handleJobQueueEvent, selectedJob, updateJobList, updateSortKey)
import UI.Poller (handlePollerEvent, tailFile)
import qualified UI.Transient as TR

scontrolTransient :: TR.TransientState SlewEvent
scontrolTransient =
    TR.menu "Job Control" $
        mconcat
            [ TR.submenu 's' "State Control" $
                mconcat
                    [ TR.item 'h' "Hold" (SControl Hold)
                    , TR.item 'r' "Resume" (SControl Resume)
                    , TR.item 's' "Suspend" (SControl Suspend)
                    , TR.item 'c' "Cancel" (SControl Cancel)
                    ]
            , TR.submenu 'p' "Priority" $
                mconcat
                    [ TR.item 't' "Top" (SControl Top)
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
handleEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
    msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
    case msg of
        Just TR.Close -> transient .= Nothing
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
handleEvent (AppEvent (SControl Cancel)) = shellWithJob cancelCmd
  where
    cancelCmd job = "scancel " +| job ^. jobId |+ ""
handleEvent (AppEvent (SControl Hold)) = shellWithJob (scontrol "hold")
handleEvent (AppEvent (SControl Resume)) = shellWithJob (scontrol "resume")
handleEvent (AppEvent (SControl Suspend)) = shellWithJob (scontrol "suspend")
handleEvent (AppEvent (SControl Release)) = shellWithJob (scontrol "release")
handleEvent (AppEvent (SControl Top)) = shellWithJob topCmd
  where
    topCmd job = "scontrol update job=" +| job ^. jobId |+ " priority=Top"
handleEvent (AppEvent (PollEvent ev)) = zoom pollState (handlePollerEvent ev)
handleEvent _ = pure ()

------------------------------------------------------------
-- Helpers

exec :: (MonadIO m) => String -> m ()
exec cmd = liftIO $ do
    cmdHandle <- spawnCommand cmd
    void $ waitForProcess cmdHandle

shellWithJob :: (Job -> String) -> EventM Name AppState ()
shellWithJob f = do
    jobMay <- preuse (jobQueueState . selectedJob)
    case jobMay of
        Just job ->
            do
                (exec . f) job
        Nothing -> pure ()

scontrol :: String -> Job -> String
scontrol verb job = "scontrol " +| verb |+ " " +| job ^. jobId |+ ""
