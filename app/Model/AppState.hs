module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Category (..),
    Name (..),
    View (..),
    Filter (..),
    initialState,
) where

import Brick.BChan (BChan)
import Model.Job (Job)
import Model.Options (Options)
import Model.SQueue (SlurmResponse)
import Model.SlurmCommand (SlurmCommand, SlurmCommandResult (..), squeueMe)
import Model.TimingState (TimingState, initialTimingState)
import Model.ViewState (View (..), ViewState, initialViewState)
import Optics.Label ()
import Slurm.Channel (SlurmRequest)
import UI.Echo (EchoState, echoState)
import UI.JobList (JobQueueState, jobList)
import UI.SlurmCommand (
    SlurmCommandLogState,
    scontrolLog,
 )
import UI.Transient qualified as TR

------------------------------------------------------------
-- Event Messages

data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data Filter = User | NoFilter deriving (Show)
data SlewEvent = SQueueStatus (SlurmCommandResult SlurmResponse) | SlurmCommandSend (Job -> SlurmCommand) | SlurmCommandReceive (SlurmCommandResult ()) | SortBy Category | FilterBy Filter | TriggerSQueue | Tick
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
    , squeueCommand :: SlurmCommand
    , worker :: BChan (SlurmRequest SlewEvent)
    }
    deriving (Generic)

------------------------------------------------------------
-- Initial State

initialState :: BChan (SlurmRequest SlewEvent) -> Options -> IO AppState
initialState chan options = do
    timing <- initialTimingState
    return $
        AppState
            { jobQueueState = jobList SearchEditor JobListWidget
            , transient = Nothing
            , scontrolLogState = scontrolLog SlurmCommandLogWidget
            , timingState = timing
            , echoState = echoState
            , viewState = initialViewState
            , options = options
            , squeueCommand = squeueMe
            , worker = chan
            }
