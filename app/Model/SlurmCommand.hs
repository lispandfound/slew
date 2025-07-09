{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.SlurmCommand (
    SlurmCommandError (..),
    SlurmCommandOutput (..),
    cancelJob,
    requeueJob,
    requeueHoldJob,
    holdJob,
    userHoldJob,
    releaseJob,
    resumeJob,
    suspendJob,
    topJob,
    notifyJob,
    writeBatchScript,
    waitJob,
    runSlurmCommand,
) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data SlurmCommandError = SlurmCommandError {cmd :: String, args :: [String], exitCode :: Int, stdout :: String, stderr :: String} deriving (Show)
data SlurmCommandOutput = SlurmCommandOutput {cmd :: String, args :: [String], stdout :: String, stderr :: String} deriving (Show)

cancelJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
cancelJob ids = runShell "scancel" (map show ids)

requeueJob :: Int -> IO (Either SlurmCommandError SlurmCommandOutput)
requeueJob jid = runSlurmCommand ["requeue", show jid]

requeueHoldJob :: Int -> IO (Either SlurmCommandError SlurmCommandOutput)
requeueHoldJob jid = runSlurmCommand ["requeuehold", show jid]

holdJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
holdJob ids = runSlurmCommand $ ["hold"] <> map show ids

userHoldJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
userHoldJob ids = runSlurmCommand $ ["uhold"] <> map show ids

releaseJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
releaseJob ids = runSlurmCommand $ ["release"] <> map show ids

resumeJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
resumeJob ids = runSlurmCommand $ ["resume"] <> map show ids

suspendJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
suspendJob ids = runSlurmCommand $ ["suspend"] <> map show ids

topJob :: [Int] -> IO (Either SlurmCommandError SlurmCommandOutput)
topJob ids = runSlurmCommand $ ["top"] <> map show ids

notifyJob :: Int -> String -> IO (Either SlurmCommandError SlurmCommandOutput)
notifyJob jid msg = runSlurmCommand ["notify", show jid, msg]

writeBatchScript :: Int -> Maybe FilePath -> IO (Either SlurmCommandError SlurmCommandOutput)
writeBatchScript jid fname = runSlurmCommand $ ["write", "batch_script", show jid] <> maybeToList fname

waitJob :: Int -> IO (Either SlurmCommandError SlurmCommandOutput)
waitJob jid = runSlurmCommand ["wait_job", show jid]

-- | Run an SlurmCommand command and return either error or output.
runSlurmCommand :: [String] -> IO (Either SlurmCommandError SlurmCommandOutput)
runSlurmCommand = runShell "scontrol"

runShell :: String -> [String] -> IO (Either SlurmCommandError SlurmCommandOutput)
runShell cmd' args = do
    (code, out, err) <- readProcessWithExitCode cmd' args ""
    pure $ case code of
        ExitSuccess -> Right (SlurmCommandOutput cmd' args out err)
        ExitFailure num -> Left (SlurmCommandError cmd' args num out err)
