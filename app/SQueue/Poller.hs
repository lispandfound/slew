{-# LANGUAGE NumericUnderscores #-}

module SQueue.Poller (squeueThread) where

import Brick.BChan as BC (BChan, readBChan)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Data.Aeson (eitherDecode)
import Model.Job (Job)
import Model.SQueue (jobs)
import System.Process (readCreateProcess, shell)

pollJobs :: IO (Either String [Job])
pollJobs = do
    squeueFile <- readCreateProcess (shell "squeue --json") ""
    return . second jobs . eitherDecode . encodeUtf8 $ squeueFile

squeueThread :: Int -> BChan () -> ([Job] -> IO ()) -> IO void
squeueThread pollingInterval promptChannel callback = forever $ do
    _jobs <- pollJobs
    case _jobs of
        Right sqStatus -> callback sqStatus
        Left err -> putStrLn err >> pure ()
    race (readBChan promptChannel) (threadDelay (pollingInterval * 1_000_000))
