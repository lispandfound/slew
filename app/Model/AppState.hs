module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Command (..),
    Category (..),
    Name (..),
    initialState,
) where

import Brick.BChan (BChan, newBChan)
import Control.Concurrent.STM.TChan (newTChanIO)
import Data.Time.Clock.System (SystemTime, getSystemTime)
import Model.Job (Job)
import Optics.Label ()
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
data SlewEvent = SQueueStatus [Job] | PollEvent UP.PollEvent | SlurmCommandSend Command | SlurmCommandReceive SlurmCommandLogEntry | SortBy Category | Tick deriving (Show)

data Name = SearchEditor | JobListWidget | SlurmCommandLogView
    deriving (Eq, Ord, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { jobQueueState :: JobQueueState Name
    , transient :: Maybe (TR.TransientState SlewEvent)
    , pollState :: PollerState
    , scontrolLogState :: SlurmCommandLogState Name
    , squeueChannel :: BChan ()
    , currentTime :: SystemTime
    , showLog ::
        Bool
    }
    deriving (Generic)

------------------------------------------------------------
-- Initial State

initialState :: IO AppState
initialState = do
    tailCommandChannel <- newTChanIO
    scontrolCommandChannel <- newBChan 10
    squeueChannel' <- newBChan 10
    currentTime' <- getSystemTime
    return $
        AppState
            { jobQueueState = jobList SearchEditor JobListWidget
            , transient = Nothing
            , squeueChannel = squeueChannel'
            , pollState = poller tailCommandChannel 10
            , scontrolLogState = scontrolLog SlurmCommandLogView scontrolCommandChannel
            , showLog = False
            , currentTime = currentTime'
            }
