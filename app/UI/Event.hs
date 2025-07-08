{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Event (
    handleEvent,
    handleSearchEvent,
    shellWithJob,
) where

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.List
import Control.Lens hiding (zoom)
import qualified Data.Vector as Vec
import Fmt
import qualified Graphics.Vty as V
import System.Process (spawnCommand, waitForProcess)

import Model.AppState
import Model.Job
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

sortListByCat :: Category -> [Job] -> [Job]
sortListByCat Account = sortOn (view account)
sortListByCat CPUs = sortOn (view cpus)
sortListByCat StartTime = sortOn (view startTime)
sortListByCat EndTime = sortOn (view endTime)
sortListByCat JobName = sortOn (view name)
sortListByCat UserName = sortOn (view userName)
sortListByCat Memory = sortOn (view memoryPerNode)

updateList :: [Job] -> EventM Name AppState ()
updateList jobs = do
    currentSortKey <- use sortKey
    let sortedJobs = maybe jobs (`sortListByCat` jobs) currentSortKey
    jobList %= listReplace (Vec.fromList sortedJobs) (Just 0)
    allJobs .= sortedJobs
    searchJobList

-- | Main event handler
handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
    msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
    case msg of
        Just TR.Close -> transient .= Nothing
        _ -> halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent e@(V.EvKey V.KUp [])) = zoom jobList (handleListEvent e)
handleEvent (VtyEvent e@(V.EvKey V.KDown [])) = zoom jobList (handleListEvent e)
handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
    transient .= Just scontrolTransient -- could parameterise this
handleEvent (VtyEvent (V.EvKey (V.KChar 'o') [V.MCtrl])) = do
    curFile <- use (selectedJob . standardOutput)
    pollTitle .= Just (fmt $ "Stdout: " +| toText curFile |+ "")
    zoom pollState (tailFile curFile)
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) =
    transient .= Just sortTransient -- could parameterise this
handleEvent (VtyEvent e) = do
    msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
    case msg of
        Just TR.Close -> transient .= Nothing
        Just (TR.Msg msg') -> transient .= Nothing >> handleEvent (AppEvent msg')
        Just TR.Next -> pure ()
        _ -> handleSearchEvent e
handleEvent (AppEvent (SQueueStatus jobs)) = updateList jobs
handleEvent (AppEvent (SortBy category)) = do
    sortKey .= Just category
    jobs <- use allJobs
    updateList jobs
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

-- | Apply search to job list
searchJobList :: EventM Name AppState ()
searchJobList = do
    st <- get
    put $ updateJobList (st ^. currentSearchTerm) st

-- | Handle search editor input
handleSearchEvent :: V.Event -> EventM Name AppState ()
handleSearchEvent e = do
    zoom searchEditor $ handleEditorEvent (VtyEvent e)
    searchJobList

exec :: (MonadIO m) => String -> m ()
exec cmd = liftIO $ do
    cmdHandle <- spawnCommand cmd
    void $ waitForProcess cmdHandle

shellWithJob :: (Job -> String) -> EventM Name AppState ()
shellWithJob f = do
    jobMay <- preuse selectedJob
    case jobMay of
        Just job ->
            do
                (exec . f) job
        Nothing -> pure ()

scontrol :: String -> Job -> String
scontrol verb job = "scontrol " +| verb |+ " " +| job ^. jobId |+ ""
