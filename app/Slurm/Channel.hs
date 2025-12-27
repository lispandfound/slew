module Slurm.Channel (worker, runJson, runJsonErr, runDiscard, SlurmRequest) where

import Brick.BChan (BChan, readBChan, writeBChan)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString (hGetContents)
import Model.SlurmCommand (ProcessRunner, SlurmCommandResult (..), SlurmContext (..), SlurmError (..), SlurmRequest (..), Stream (..))
import Optics.Setter (over)
import System.Exit (ExitCode (..))
import System.Process (
    CmdSpec (RawCommand, ShellCommand),
    CreateProcess (cmdspec, std_err, std_out),
    StdStream (CreatePipe),
    waitForProcess,
    withCreateProcess,
 )
import System.Timeout (timeout)

commandName :: CmdSpec -> String
commandName (ShellCommand s) = s
commandName (RawCommand c _) = c

commandArgs :: CmdSpec -> [String]
commandArgs (ShellCommand _) = []
commandArgs (RawCommand _ a) = a

context :: CmdSpec -> ByteString -> ByteString -> SlurmContext
context procSpec out err = SlurmContext (commandName procSpec) (commandArgs procSpec) (decodeUtf8 out) (decodeUtf8 err)

resultFor :: ExitCode -> Stream -> Either SlurmError ByteString -> Either SlurmError ByteString -> Either SlurmError ByteString
resultFor (ExitFailure c) _ _ _ = Left (ExecutionError c)
resultFor _ Stdout out _ = out
resultFor _ Stderr _ err = err

seconds :: Int
seconds = 1_000_000

readOrTimeout :: (MonadIO m) => Int -> Handle -> m (Either SlurmError ByteString)
readOrTimeout maxTime handle = maybeToRight TimeoutError <$> liftIO (timeout maxTime (hGetContents handle))

runSlurmProcess ::
    (MonadIO m) =>
    ProcessRunner m ->
    Stream ->
    CreateProcess ->
    m (SlurmCommandResult ByteString)
runSlurmProcess runner stream cp = runner cp $ \mOut mErr _ ph -> do
    outBytes <- maybe (return $ Left (BrokenHandle Stdout)) (readOrTimeout (60 * seconds)) mOut
    errBytes <- maybe (return $ Left (BrokenHandle Stderr)) (readOrTimeout (60 * seconds)) mErr
    exitStatus <- liftIO $ waitForProcess ph
    return $
        SlurmCommandResult
            { context = context (cmdspec cp) (fromRight mempty outBytes) (fromRight mempty errBytes)
            , result = resultFor exitStatus stream outBytes errBytes
            }

worker :: BChan (SlurmRequest b) -> BChan b -> IO ()
worker input output = forever $ do
    SlurmRequest cp stream cb <- readBChan input
    let cp' = cp{std_out = CreatePipe, std_err = CreatePipe}
    result <- runSlurmProcess withCreateProcess stream cp'
    writeBChan output (cb result)

parseJson :: (FromJSON a) => SlurmCommandResult ByteString -> SlurmCommandResult a
parseJson = over #result join . fmap (first (DecodingError . toText) . eitherDecode . toLazy)

runJsonFrom :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> Stream -> (SlurmCommandResult a -> b) -> IO ()
runJsonFrom chan process stream callback = writeBChan chan $ SlurmRequest process stream (callback . parseJson)

runJsonErr :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> (SlurmCommandResult a -> b) -> IO ()
runJsonErr chan process = runJsonFrom chan process Stderr

runJson :: (FromJSON a) => BChan (SlurmRequest b) -> CreateProcess -> (SlurmCommandResult a -> b) -> IO ()
runJson chan process = runJsonFrom chan process Stdout

runDiscard :: BChan (SlurmRequest a) -> CreateProcess -> (SlurmCommandResult () -> a) -> IO ()
runDiscard chan process callback = writeBChan chan $ SlurmRequest process Stdout (callback . void)
