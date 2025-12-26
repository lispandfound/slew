module Slurm.Channel where

import Brick.BChan
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (hGetContents)
import qualified Data.Text.Lazy.Encoding as TLE
import Fmt
import Model.SlurmCommand (SlurmCommandResult (..), SlurmContext (..), SlurmError (..))
import Optics.Setter (over)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process

data SlurmRequest b = SlurmRequest
    { cp :: CreateProcess
    , callback :: SlurmCommandResult LByteString -> b
    }

worker :: BChan (SlurmRequest b) -> BChan b -> IO ()
worker input output = forever $ do
    SlurmRequest procSpec cb <- readBChan input
    traceM "Receiving a command"
    -- Extract metadata for our data types
    let (commandName, commandArgs) = case cmdspec procSpec of
            ShellCommand s -> (s, [])
            RawCommand c a -> (c, a)
    let pSet = procSpec{std_out = CreatePipe, std_err = CreatePipe}
    result <- withCreateProcess pSet $ \mOut mErr _ ph -> do
        traceM "Waiting for process"
        exitStatus <- liftIO $ waitForProcess ph
        traceM "Process done"
        outBytes <- liftIO $ maybe (return mempty) hGetContents mOut
        errText <- liftIO $ maybe (return mempty) (fmap decodeUtf8 . hGetContents) mErr
        traceM . fmt $ "Ran " +| commandName |+ " " +| unwordsF commandArgs

        traceM . fmt $ "Output: " +| (decodeUtf8 outBytes :: Text) |+ ""
        traceM . fmt $ "Err: " +| errText |+ ""

        case exitStatus of
            ExitSuccess ->
                return $
                    SlurmCommandResult
                        ( SlurmContext
                            { cmd = commandName
                            , args = commandArgs
                            , stdout = decodeUtf8 outBytes
                            , stderr = errText
                            }
                        )
                        (Right outBytes)
            ExitFailure code ->
                return $
                    SlurmCommandResult
                        ( SlurmContext
                            { cmd = commandName
                            , args = commandArgs
                            , stdout = decodeUtf8 outBytes
                            , stderr = errText
                            }
                        )
                        (Left (ExecutionError code))

    traceM "Sending output"

    writeBChan output (cb result)

---

runJson :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> (SlurmCommandResult a -> b) -> IO ()
runJson chan process callback = writeBChan chan $ SlurmRequest process (callback . parseJson)
  where
    parseJson :: (FromJSON a) => SlurmCommandResult LByteString -> SlurmCommandResult a
    parseJson = over #result join . fmap (first (DecodingError . toText) . eitherDecode)

runDiscard :: BChan (SlurmRequest a) -> CreateProcess -> (SlurmCommandResult () -> a) -> IO ()
runDiscard chan process callback = writeBChan chan $ SlurmRequest process (callback . void)
