{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Command (..),
    Category (..),
    Name (..),
    squeueChannel,
    jobQueueState,
    transient,
    pollState,
    initialState,
    scontrolLogState,
    showLog,
    triggerSqueue,
) where

import Brick (EventM)
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent.STM.TChan (newTChanIO)
import Control.Lens (makeLenses, use)
import Model.Job (Job)
import UI.JobList (JobQueueState, jobList)
import UI.Poller (PollerState, poller)
import qualified UI.Poller as UP
import UI.SlurmCommand (
    SlurmCommandLogEntry,
    SlurmCommandLogState,
    scontrolLog,
 )
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show)
data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data SlewEvent = SQueueStatus [Job] | PollEvent UP.PollEvent | SlurmCommandSend Command | SlurmCommandReceive SlurmCommandLogEntry | SortBy Category deriving (Show)

data Name = SearchEditor | JobListWidget | SlurmCommandLogView
    deriving (Eq, Ord, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { _jobQueueState :: JobQueueState Name
    , _transient :: Maybe (TR.TransientState SlewEvent)
    , _pollState :: PollerState
    , _scontrolLogState :: SlurmCommandLogState Name
    , _squeueChannel :: BChan ()
    , _showLog :: Bool
    }

makeLenses ''AppState

------------------------------------------------------------
-- Initial State

triggerSqueue :: EventM n AppState ()
triggerSqueue = do
    ch <- use squeueChannel
    liftIO (writeBChan ch ())

initialState :: IO AppState
initialState = do
    tailCommandChannel <- newTChanIO
    scontrolCommandChannel <- newBChan 10
    squeueChannel' <- newBChan 10
    return $
        AppState
            { _jobQueueState = jobList SearchEditor JobListWidget
            , _transient = Nothing
            , _squeueChannel = squeueChannel'
            , _pollState = poller tailCommandChannel 10
            , _scontrolLogState = scontrolLog SlurmCommandLogView scontrolCommandChannel
            , _showLog = False
            }
