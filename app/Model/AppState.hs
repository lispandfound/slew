{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Command (..),
    Category (..),
    Name (..),
    jobQueueState,
    transient,
    pollState,
    initialState,
) where

import Control.Concurrent.STM.TChan (TChan)
import Control.Lens (makeLenses)
import Model.Job (Job)
import qualified Tail.Poller as P
import UI.JobList (JobQueueState, jobList)
import UI.Poller (PollerState, poller)
import qualified UI.Poller as UP
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show)
data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data SlewEvent = SQueueStatus [Job] | PollEvent UP.PollEvent | SControl Command | SortBy Category deriving (Show)

data Name = SearchEditor | JobListWidget
    deriving (Eq, Ord, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { _jobQueueState :: JobQueueState Name
    , _transient :: Maybe (TR.TransientState SlewEvent)
    , _pollState :: PollerState
    }

makeLenses ''AppState

------------------------------------------------------------
-- Initial State

initialState :: TChan P.Command -> AppState
initialState chan =
    AppState
        { _jobQueueState = jobList SearchEditor JobListWidget
        , _transient = Nothing
        , _pollState = poller chan 10
        }
