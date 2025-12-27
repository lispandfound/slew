{-# LANGUAGE DuplicateRecordFields #-}

module Model.SlurmCommand (
    SlurmCommandResult (..),
    SlurmError (..),
    SlurmContext (..),
    Command (..),
    SlurmRequest (..),
    ProcessRunner,
    Stream (..),
    slurm,
    squeue,
) where

import Optics.Label ()
import System.Process (CreateProcess, ProcessHandle, proc)

data Stream = Stdout | Stderr deriving (Show)
data SlurmRequest b = SlurmRequest CreateProcess Stream (SlurmCommandResult ByteString -> b)

type ProcessRunner m =
    forall a.
    CreateProcess ->
    (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> m a) ->
    m a

data SlurmError
    = ExecutionError {exitCode :: Int}
    | DecodingError {parseError :: Text}
    | TimeoutError
    | BrokenHandle Stream
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

data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show)

slurm :: Command -> Int -> CreateProcess
slurm Cancel job = proc "scancel" [show job]
slurm Suspend job = proc "scontrol" ["suspend", show job]
slurm Resume job = proc "scontrol" ["resume", show job]
slurm Hold job = proc "scontrol" ["hold", show job]
slurm Release job = proc "scontrol" ["release", show job]
slurm Top job = proc "scontrol" ["top", show job]

squeue :: CreateProcess
squeue = proc "squeue" ["--json"]
