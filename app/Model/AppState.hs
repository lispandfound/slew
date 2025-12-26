module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Command (..),
    Category (..),
    Name (..),
    View (..),
    initialState,
) where

import Brick.BChan (BChan)
import Data.Time.Clock.System (SystemTime, getSystemTime)
import Model.Job (Job)
import Model.Options (Options)
import Model.SlurmCommand (Command (..), SlurmCommandResult (..))
import Optics.Label ()
import Slurm.Channel (SlurmRequest)
import UI.Echo (EchoState, echoStateWith)
import UI.JobList (JobQueueState, jobList)
import UI.SlurmCommand (
    SlurmCommandLogState,
    scontrolLog,
 )
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data SlewEvent = SQueueStatus (SlurmCommandResult [Job]) | SlurmCommandSend Command | SlurmCommandReceive (SlurmCommandResult ()) | SortBy Category | Tick deriving (Show)
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
    , currentTime :: SystemTime
    , lastUpdate :: Maybe SystemTime
    , echoState :: EchoState
    , view :: NonEmpty View
    , options :: Options
    , worker :: BChan (SlurmRequest SlewEvent)
    }
    deriving (Generic)

------------------------------------------------------------
-- Initial State

initialMessage :: Text
initialMessage = "type C-c to interact with slurm jobs, C-l to view logs, C-s to sort, and C-o to tail job output"

initialState :: BChan (SlurmRequest SlewEvent) -> Options -> IO AppState
initialState chan options = do
    currentTime' <- getSystemTime
    return $
        AppState
            { jobQueueState = jobList SearchEditor JobListWidget
            , transient = Nothing
            , scontrolLogState = scontrolLog SlurmCommandLogWidget
            , currentTime = currentTime'
            , lastUpdate = Nothing
            , echoState = echoStateWith initialMessage
            , view = SQueueView :| []
            , options = options
            , worker = chan
            }
