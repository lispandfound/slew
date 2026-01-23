{-# LANGUAGE DuplicateRecordFields #-}

module Model.SlurmCommand (
    SlurmCommandResult (..),
    SlurmError (..),
    SlurmContext (..),
    SlurmRequest (..),
    ProcessRunner,
    Stream (..),
    SlurmCommand,
    toProc,
    squeue,
    squeueMe,
    squeueAll,
    cancel,
    suspend,
    top,
    hold,
    resume,
    release,
) where

import Model.Job (Job (..))
import Optics.Getter (view)
import Optics.Label ()
import System.Process (CreateProcess, ProcessHandle, proc)

data Stream = Stdout | Stderr deriving (Show)
data SlurmRequest b = SlurmRequest CreateProcess Stream (SlurmCommandResult ByteString -> b)
newtype SlurmCommand = SlurmCommand {cmd :: CreateProcess} deriving (Generic, Show)

toProc :: SlurmCommand -> CreateProcess
toProc = view #cmd

type ProcessRunner m =
    forall a.
    CreateProcess ->
    (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> m a) ->
    m a

data SlurmError
    = ExecutionError {exitCode :: Int}
    | DecodingError {parseError :: Text}
    deriving (Show, Generic)

data SlurmContext = SlurmContext
    { cmd :: String
    , args :: [String]
    , stdout :: Text
    , stderr :: Text
    }
    deriving (Show, Generic)

data SlurmCommandResult a = SlurmCommandResult
    { context :: SlurmContext
    , result :: Either SlurmError a
    }
    deriving (Show, Generic, Functor)

withJobId :: String -> [String] -> Job -> SlurmCommand
withJobId cmd args = SlurmCommand . proc cmd . (args <>) . pure . show . view #jobId

cancel :: Job -> SlurmCommand
cancel = withJobId "scancel" []

suspend :: Job -> SlurmCommand
suspend = withJobId "scontrol" ["suspend"]

resume :: Job -> SlurmCommand
resume = withJobId "scontrol" ["resume"]

hold :: Job -> SlurmCommand
hold = withJobId "scontrol" ["hold"]

release :: Job -> SlurmCommand
release = withJobId "scontrol" ["release"]

top :: Job -> SlurmCommand
top = withJobId "scontrol" ["top"]

squeue :: [String] -> SlurmCommand
squeue args = SlurmCommand $ proc "squeue" ("--json" : args)

squeueAll :: SlurmCommand
squeueAll = squeue []

squeueMe :: SlurmCommand
squeueMe = squeue ["--me"]
