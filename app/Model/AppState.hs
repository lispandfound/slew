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
import Model.Options (Options)
import Model.SQueue (SlurmResponse)
import Model.SlurmCommand (Command (..), SlurmCommandResult (..))
import Model.TimingState (TimingState, initialTimingState)
import Model.ViewState (View (..), ViewState, initialViewState)
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
data SlewEvent = SQueueStatus (SlurmCommandResult SlurmResponse) | SlurmCommandSend Command | SlurmCommandReceive (SlurmCommandResult ()) | SortBy Category | Tick deriving (Show)
data Name = SearchEditor | JobListWidget | SlurmCommandLogWidget | TransientWidget
    deriving (Eq, Ord, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { jobQueueState :: JobQueueState Name
    , transient :: Maybe (TR.TransientState SlewEvent Name)
    , scontrolLogState :: SlurmCommandLogState Name
    , timingState :: TimingState
    , echoState :: EchoState
    , viewState :: ViewState
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
    timing <- initialTimingState
    return $
        AppState
            { jobQueueState = jobList SearchEditor JobListWidget
            , transient = Nothing
            , scontrolLogState = scontrolLog SlurmCommandLogWidget
            , timingState = timing
            , echoState = echoStateWith initialMessage
            , viewState = initialViewState
            , options = options
            , worker = chan
            }
