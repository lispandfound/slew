module UI.Poller (tailFile) where

import Brick (EventM, suspendAndResume')
import Control.Exception (AsyncException (..), catch)
import Data.Text (replace)
import Fmt (fmt, (+|), (|+))
import GHC.IO.Handle (hGetContents')
import System.Exit (ExitCode (..))
import System.Process (CreateProcess (..), StdStream (CreatePipe), shell, waitForProcess, withCreateProcess)

safeCall :: String -> IO (Either Text ())
safeCall cmd = do
    (code, err) <- withCreateProcess ((shell cmd){std_err = CreatePipe, delegate_ctlc = True}) $ \_ _ errorHandle processHandle -> do
        code <- waitForProcess processHandle `catch` userInterrupt
        err <- maybe (pure "") hGetContents' errorHandle
        return (code, err)
    case code of
        ExitSuccess -> return (Right ())
        ExitFailure intCode -> return . Left . fmt $ "Program " +| cmd |+ " exited with code " +| intCode |+ ": " +| err |+ ""
  where
    userInterrupt :: AsyncException -> IO ExitCode
    -- This is always a user interrupt per the documentation. Users
    -- terminating their own programs is not an error.
    userInterrupt _ = pure ExitSuccess

tailFile :: (Ord n) => FilePath -> Text -> EventM n s (Either Text ())
tailFile fp = suspendAndResume' . safeCall . toString . replace "%f" (toText fp)
