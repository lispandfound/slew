module UI.Poller (tailFile) where

import Brick (EventM, suspendAndResume')
import Fmt (fmt, (+|), (|+))
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode, showCommandForUser)

safeCall :: FilePath -> [String] -> IO (Either Text ())
safeCall prog args = do
    (code, _, err) <- readCreateProcessWithExitCode (proc prog args) mempty
    case code of
        ExitSuccess -> return (Right ())
        ExitFailure intCode -> return . Left . fmt $ "Program " +| showCommandForUser prog args |+ " exited with code " +| intCode |+ ": " +| err |+ ""

tailFile :: (Ord n) => FilePath -> EventM n s (Either Text ())
tailFile fp = suspendAndResume' $ safeCall "less" ["+F", fp]
