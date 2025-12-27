{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Model.SlurmCommand (
    SlurmCommandResult (..),
    SlurmError (..),
    SlurmContext (..),
    Command (..),
    slurm,
    squeue,
) where

import Optics.Label ()
import System.Process (CreateProcess, proc)

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
