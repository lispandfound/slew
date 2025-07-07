module SQueue.Poller (squeueThread) where

import Brick.BChan as BC
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Data.Aeson (eitherDecode)
import Model.Job (Job)
import Model.SQueue (jobs)
import System.Process (readCreateProcess, shell)

pollJobs :: IO (Either String [Job])
pollJobs = do
    squeueFile <- readCreateProcess (shell "squeue --json") ""
    return . second jobs . eitherDecode . encodeUtf8 $ squeueFile

squeueThread :: ([Job] -> a) -> BC.BChan a -> IO ThreadId
squeueThread f chan = forkIO $ do
    forever $ do
        _jobs <- pollJobs
        case _jobs of
            Right sqStatus -> BC.writeBChan chan . f $ sqStatus
            Left err -> putStrLn err >> pure ()
        threadDelay 30000000
