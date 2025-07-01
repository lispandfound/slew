{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Event
  ( handleEvent
  , handleSearchEvent
  , handleTransientMsg
  , shellWithJob
  ) where

import Brick
import Brick.Widgets.Edit
import Brick.Widgets.List
import qualified Graphics.Vty as V
import Control.Lens hiding (zoom)
import qualified Data.Vector as Vec
import System.Process (spawnCommand, waitForProcess)
import Fmt

import qualified UI.Transient as TR
import Model.AppState
import Model.Job

scontrolTransient :: TR.TransientState TransientMsg
scontrolTransient = TR.menu "Job Control" $ mconcat
  [ TR.submenu 's' "State Control" $ mconcat
      [ TR.item 'h' "Hold" Hold
      , TR.item 'r' "Resume" Resume
      , TR.item 's' "Suspend" Suspend
      , TR.item 'c' "Cancel" Cancel
      ]
  , TR.submenu 'p' "Priority" $ mconcat
      [ TR.item 't' "Top" Top
      ]
  ]


-- | Main event handler
handleEvent :: BrickEvent Name SlewEvent -> EventM Name AppState ()
handleEvent (VtyEvent e@(V.EvKey V.KEsc [])) = do
  msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
  case msg of
    Just TR.Close -> transient .= Nothing
    _             -> halt

handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt

handleEvent (VtyEvent e@(V.EvKey V.KUp []))   = zoom jobList (handleListEvent e) >> selectJob
handleEvent (VtyEvent e@(V.EvKey V.KDown [])) = zoom jobList (handleListEvent e) >> selectJob

handleEvent (VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) =
  transient .= Just scontrolTransient -- could parameterise this

handleEvent (VtyEvent e) = do
  msg <- getFirst <$> zoom (transient . _Just) (TR.handleTransientEvent e)
  case msg of
    Just TR.Close      -> transient .= Nothing
    Just (TR.Msg msg') -> transient .= Nothing >> handleTransientMsg msg'
    Just TR.Next          -> pure ()
    _                  -> handleSearchEvent e >> selectJob

handleEvent (AppEvent (SQueueStatus jobs)) = do
  jobList %= listReplace (Vec.fromList jobs) (Just 0)
  allJobs .= jobs
  searchJobList

handleEvent _ = pure ()

------------------------------------------------------------
-- Helpers

-- | Apply search to job list
searchJobList :: EventM Name AppState ()
searchJobList = do
  st <- get
  put $ updateJobList (getCurrentSearchTerm st) st

-- | Handle search editor input
handleSearchEvent :: V.Event -> EventM Name AppState ()
handleSearchEvent e = do
  zoom searchEditor $ handleEditorEvent (VtyEvent e)
  searchJobList

-- | Update selectedJob lens from list

selectJob :: EventM Name AppState ()
selectJob = do
    selectedElement <- preuse (jobList . listSelectedElementL)
    selectedJob .= selectedElement


exec :: MonadIO m => String -> m ()
exec cmd = liftIO $ do
  cmdHandle <- spawnCommand cmd
  void $ waitForProcess cmdHandle

shellWithJob :: (Job -> String) -> EventM Name AppState ()
shellWithJob f = do
  jobMay <- gets getSelectedJob
  case jobMay of
    Just job ->
      do
        (exec . f) job

    Nothing -> pure ()


scontrol :: String -> Job -> String
scontrol verb job = "scontrol " +| verb |+ " " +| jobId job |+ ""

handleTransientMsg :: TransientMsg -> EventM Name AppState ()
handleTransientMsg Cancel = shellWithJob cancelCmd
  where cancelCmd job = "scancel " +| jobId job |+ ""
handleTransientMsg Hold = shellWithJob (scontrol "hold")
handleTransientMsg Resume = shellWithJob (scontrol "resume")
handleTransientMsg Suspend = shellWithJob (scontrol "suspend")
handleTransientMsg Release = shellWithJob (scontrol "release")
handleTransientMsg Top = shellWithJob topCmd
  where topCmd job = "scontrol update job=" +| jobId job |+ " priority=Top"
