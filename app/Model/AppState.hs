module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Category (..),
    Name (..),
    View (..),
    initialState,
) where

import Brick.BChan (BChan, newBChan)
import Data.Time.Clock.System (SystemTime, getSystemTime)
import Logic.EventHandlers (Command)
import Logic.JobFiltering (Category (..))
import Model.Job (Job)
import Model.Options (Options)
import Optics.Label ()
import UI.Echo (EchoState, echoStateWith)
import UI.JobList (JobQueueState, jobList)
import UI.SlurmCommand (
    SlurmCommandLogEntry,
    SlurmCommandLogState,
    scontrolLog,
 )
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data SlewEvent = SQueueStatus [Job] | SlurmCommandSend Command | SlurmCommandReceive SlurmCommandLogEntry | SortBy Category | Tick deriving (Show)

data Name = SearchEditor | JobListWidget | SlurmCommandLogWidget | TransientWidget
    deriving (Eq, Ord, Show)

data View = SQueueView | CommandLogView | NodeView
    deriving (Eq, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { jobQueueState :: JobQueueState Name
    , transient :: Maybe (TR.TransientState SlewEvent Name)
    , scontrolLogState :: SlurmCommandLogState Name
    , squeueChannel :: BChan ()
    , currentTime :: SystemTime
    , lastUpdate :: Maybe SystemTime
    , echoState :: EchoState
    , view :: NonEmpty View
    , options :: Options
    }
    deriving (Generic)

------------------------------------------------------------
-- Initial State

initialMessage :: Text
initialMessage = "type C-c to interact with slurm jobs, C-l to view logs, C-s to sort, and C-o to tail job output"

initialState :: Options -> IO AppState
initialState options = do
    scontrolCommandChannel <- newBChan 10
    squeueChannel' <- newBChan 10
    currentTime' <- getSystemTime
    return $
        AppState
            { jobQueueState = jobList SearchEditor JobListWidget
            , transient = Nothing
            , squeueChannel = squeueChannel'
            , scontrolLogState = scontrolLog SlurmCommandLogWidget scontrolCommandChannel
            , currentTime = currentTime'
            , lastUpdate = Nothing
            , echoState = echoStateWith initialMessage
            , view = SQueueView :| []
            , options = options
            }
