module Slurm.Channel where

import Brick.BChan
import Control.Monad.Trans.Resource
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (hGetContents)
import qualified Data.Text.Lazy.Encoding as TLE
import Fmt
import Model.SlurmCommand (SlurmCommandResult (..), SlurmContext (..), SlurmError (..))
import Optics.Setter (over)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process

data Stream = Stdout | Stderr
data SlurmRequest b = SlurmRequest
    { cp :: CreateProcess
    , read :: Stream
    , callback :: SlurmCommandResult ByteString -> b
    }

worker :: BChan (SlurmRequest b) -> BChan b -> IO ()
worker input output = forever $ do
    SlurmRequest procSpec stream cb <- readBChan input
    -- Extract metadata for our data types
    let (commandName, commandArgs) = case cmdspec procSpec of
            ShellCommand s -> (s, [])
            RawCommand c a -> (c, a)
    let pSet = procSpec{std_out = CreatePipe, std_err = CreatePipe}
    result <- withCreateProcess pSet $ \mOut mErr _ ph -> do
        outBytes <- liftIO $ maybe (return mempty) hGetContents mOut
        errBytes <- liftIO $ maybe (return mempty) hGetContents mErr
        exitStatus <- liftIO $ waitForProcess ph
        let result = case stream of
                Stdout -> outBytes
                Stderr -> errBytes
        case exitStatus of
            ExitSuccess ->
                return $
                    SlurmCommandResult
                        ( SlurmContext
                            { cmd = commandName
                            , args = commandArgs
                            , stdout = decodeUtf8 outBytes
                            , stderr = decodeUtf8 errBytes
                            }
                        )
                        (Right result)
            ExitFailure code ->
                return $
                    SlurmCommandResult
                        ( SlurmContext
                            { cmd = commandName
                            , args = commandArgs
                            , stdout = decodeUtf8 outBytes
                            , stderr = decodeUtf8 errBytes
                            }
                        )
                        (Left (ExecutionError code))
    writeBChan output (cb result)

runJsonFrom :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> Stream -> (SlurmCommandResult a -> b) -> IO ()
runJsonFrom chan process stream callback = writeBChan chan $ SlurmRequest process stream (callback . parseJson)
  where
    parseJson :: (FromJSON a) => SlurmCommandResult ByteString -> SlurmCommandResult a
    parseJson = over #result join . fmap (first (DecodingError . toText) . eitherDecode . toLazy)

runJsonErr :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> (SlurmCommandResult a -> b) -> IO ()
runJsonErr chan process = runJsonFrom chan process Stderr

runJson :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> (SlurmCommandResult a -> b) -> IO ()
runJson chan process = runJsonFrom chan process Stdout

runDiscard :: BChan (SlurmRequest a) -> CreateProcess -> (SlurmCommandResult () -> a) -> IO ()
runDiscard chan process callback = writeBChan chan $ SlurmRequest process Stdout (callback . void)
