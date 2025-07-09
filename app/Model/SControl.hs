{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.SControl (
    SControlError (..),
    SControlOutput (..),
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
    runSControl,
) where

import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

data SControlError = SControlError {cmd :: String, args :: [String], exitCode :: Int, stdout :: String, stderr :: String} deriving (Show)
data SControlOutput = SControlOutput {cmd :: String, args :: [String], stdout :: String, stderr :: String} deriving (Show)

cancelJob :: [Int] -> IO (Either SControlError SControlOutput)
cancelJob ids = runShell "scancel" (map show ids)

requeueJob :: Int -> IO (Either SControlError SControlOutput)
requeueJob jid = runSControl ["requeue", show jid]

requeueHoldJob :: Int -> IO (Either SControlError SControlOutput)
requeueHoldJob jid = runSControl ["requeuehold", show jid]

holdJob :: [Int] -> IO (Either SControlError SControlOutput)
holdJob ids = runSControl $ ["hold"] <> map show ids

userHoldJob :: [Int] -> IO (Either SControlError SControlOutput)
userHoldJob ids = runSControl $ ["uhold"] <> map show ids

releaseJob :: [Int] -> IO (Either SControlError SControlOutput)
releaseJob ids = runSControl $ ["release"] <> map show ids

resumeJob :: [Int] -> IO (Either SControlError SControlOutput)
resumeJob ids = runSControl $ ["resume"] <> map show ids

suspendJob :: [Int] -> IO (Either SControlError SControlOutput)
suspendJob ids = runSControl $ ["suspend"] <> map show ids

topJob :: [Int] -> IO (Either SControlError SControlOutput)
topJob ids = runSControl $ ["top"] <> map show ids

notifyJob :: Int -> String -> IO (Either SControlError SControlOutput)
notifyJob jid msg = runSControl ["notify", show jid, msg]

writeBatchScript :: Int -> Maybe FilePath -> IO (Either SControlError SControlOutput)
writeBatchScript jid fname = runSControl $ ["write", "batch_script", show jid] <> maybeToList fname

waitJob :: Int -> IO (Either SControlError SControlOutput)
waitJob jid = runSControl ["wait_job", show jid]

-- | Run an SControl command and return either error or output.
runSControl :: [String] -> IO (Either SControlError SControlOutput)
runSControl = runShell "scontrol"

runShell :: String -> [String] -> IO (Either SControlError SControlOutput)
runShell cmd' args = do
    (code, out, err) <- readProcessWithExitCode cmd' args ""
    pure $ case code of
        ExitSuccess -> Right (SControlOutput cmd' args out err)
        ExitFailure num -> Left (SControlError cmd' args num out err)
