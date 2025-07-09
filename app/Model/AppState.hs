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
    scontrolLogState,
    showLog,
) where

import Brick.BChan
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Lens (makeLenses)
import Model.Job (Job)
import UI.JobList (JobQueueState, jobList)
import UI.Poller (PollerState, poller)
import qualified UI.Poller as UP
import UI.SControl
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show)
data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data SlewEvent = SQueueStatus [Job] | PollEvent UP.PollEvent | SControlSend Command | SControlReceive SControlLogEntry | SortBy Category deriving (Show)

data Name = SearchEditor | JobListWidget | SControlLogView
    deriving (Eq, Ord, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { _jobQueueState :: JobQueueState Name
    , _transient :: Maybe (TR.TransientState SlewEvent)
    , _pollState :: PollerState
    , _scontrolLogState :: SControlLogState Name
    , _showLog :: Bool
    }

makeLenses ''AppState

------------------------------------------------------------
-- Initial State

initialState :: IO AppState
initialState = do
    tailCommandChannel <- newTChanIO
    scontrolCommandChannel <- newBChan 10
    return $
        AppState
            { _jobQueueState = jobList SearchEditor JobListWidget
            , _transient = Nothing
            , _pollState = poller tailCommandChannel 10
            , _scontrolLogState = scontrolLog SControlLogView scontrolCommandChannel
            , _showLog = False
            }
