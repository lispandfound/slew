{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.SlurmCommand (startSlurmCommandListener, scontrolLog, drawSlurmCommandLog, SlurmCommandCmd (..), SlurmCommandLogState, log, name, SlurmCommandLogEntry (..), result, command, chan, sendSlurmCommandCommand, logSlurmCommandEvent) where

import Brick (
    EventM,
    Padding (Pad),
    ViewportType (Vertical),
    Widget,
    attrName,
    emptyWidget,
    hBox,
    padLeftRight,
    padRight,
    str,
    txt,
    vBox,
    viewport,
    withAttr,
    (<+>),
 )
import Brick.BChan (BChan, readBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens (makeLenses, use, (%=), (^.))
import Model.SlurmCommand (
    SlurmCommandError (SlurmCommandError, exitCode, stderr),
    SlurmCommandOutput (SlurmCommandOutput, stdout),
    cancelJob,
    holdJob,
    notifyJob,
    releaseJob,
    requeueHoldJob,
    requeueJob,
    resumeJob,
    suspendJob,
    topJob,
    userHoldJob,
    waitJob,
    writeBatchScript,
 )

data SlurmCommandCmd
    = CancelJob [Int]
    | RequeueJob Int
    | RequeueHoldJob Int
    | HoldJob [Int]
    | UserHoldJob [Int]
    | ReleaseJob [Int]
    | ResumeJob [Int]
    | SuspendJob [Int]
    | TopJob [Int]
    | NotifyJob Int String
    | WriteBatchScript Int (Maybe FilePath)
    | WaitJob Int
    deriving (Eq, Show)

handleSlurmCommandCommand :: SlurmCommandCmd -> IO (Either SlurmCommandError SlurmCommandOutput)
handleSlurmCommandCommand (CancelJob ids) = cancelJob ids
handleSlurmCommandCommand (RequeueJob jid) = requeueJob jid
handleSlurmCommandCommand (RequeueHoldJob jid) = requeueHoldJob jid
handleSlurmCommandCommand (HoldJob ids) = holdJob ids
handleSlurmCommandCommand (UserHoldJob ids) = userHoldJob ids
handleSlurmCommandCommand (ReleaseJob ids) = releaseJob ids
handleSlurmCommandCommand (SuspendJob ids) = suspendJob ids
handleSlurmCommandCommand (TopJob ids) = topJob ids
handleSlurmCommandCommand (NotifyJob jid msg) = notifyJob jid msg
handleSlurmCommandCommand (WriteBatchScript jid fname) = writeBatchScript jid fname
handleSlurmCommandCommand (WaitJob jid) = waitJob jid
handleSlurmCommandCommand (ResumeJob ids) = resumeJob ids

startSlurmCommandListener :: BChan SlurmCommandCmd -> (SlurmCommandCmd -> Either SlurmCommandError SlurmCommandOutput -> IO ()) -> IO ()
startSlurmCommandListener chan callback = forever $ do
    cmd <- readBChan chan
    output <- handleSlurmCommandCommand cmd
    callback cmd output

data SlurmCommandLogEntry = SlurmCommandLogEntry
    { _command :: SlurmCommandCmd
    , _result :: Either SlurmCommandError SlurmCommandOutput
    }
    deriving (Show)

makeLenses ''SlurmCommandLogEntry

data SlurmCommandLogState n = SlurmCommandLogState
    { _log :: [SlurmCommandLogEntry]
    , _name :: n
    , _chan :: BChan SlurmCommandCmd
    }
makeLenses ''SlurmCommandLogState

scontrolLog :: n -> BChan SlurmCommandCmd -> SlurmCommandLogState n
scontrolLog name' chan' =
    SlurmCommandLogState
        { _log = mempty
        , _name = name'
        , _chan = chan'
        }

formatShellCommand :: SlurmCommandCmd -> Widget n
formatShellCommand cmd =
    case cmd of
        CancelJob ids -> format "scancel" ids
        RequeueJob jid -> format "scontrol requeue" [jid]
        RequeueHoldJob jid -> format "scontrol requeuehold" [jid]
        HoldJob ids -> format "scontrol hold" ids
        UserHoldJob ids -> format "scontrol userhold" ids
        ReleaseJob ids -> format "scontrol release" ids
        ResumeJob ids -> format "scontrol resume" ids
        SuspendJob ids -> format "scontrol suspend" ids
        TopJob ids -> format "scontrol top" ids
        NotifyJob jid msg -> format "scontrol notify" [jid] <+> str msg
        WriteBatchScript jid fname ->
            format "writebatch" [jid]
                <+> maybe emptyWidget str fname
        WaitJob jid -> format "wait" [jid]
  where
    format :: String -> [Int] -> Widget n
    format name' ids =
        padLeftRight 1 (str name')
            <+> hBox
                (map (\i -> padRight (Pad 1) $ withAttr (attrName "jobId") (txt . show $ i)) ids)

drawEntry :: SlurmCommandLogEntry -> Widget n
drawEntry (SlurmCommandLogEntry{_command = com, _result = res}) = vBox [commandLine, commandOutput]
  where
    commandLine = hBox [padRight (Pad 4) exitStatus, shellCommand]
    exitStatus = case res of
        Left (SlurmCommandError{exitCode = e}) -> withAttr (attrName "exitFailure") (txt . show $ e)
        Right _ -> withAttr (attrName "exitSuccess") (txt "0")
    commandOutput = case res of
        Left (SlurmCommandError{stderr = err}) -> str err
        Right (SlurmCommandOutput{stdout = out}) -> str out
    shellCommand = formatShellCommand com

drawSlurmCommandLog :: (Ord n, Show n) => SlurmCommandLogState n -> Widget n
drawSlurmCommandLog st = centerLayer $ borderWithLabel (txt "Slurm Command Log") $ (viewport (st ^. name) Vertical . vBox $ map drawEntry (reverse $ st ^. log))

logSlurmCommandEvent :: SlurmCommandLogEntry -> EventM n (SlurmCommandLogState n) ()
logSlurmCommandEvent output = log %= (output :)

-- NOTE: viewportScrolling is handled by name in brick so this occurs farther up the call chain in Event.hs

sendSlurmCommandCommand :: SlurmCommandCmd -> EventM n (SlurmCommandLogState n) ()
sendSlurmCommandCommand cmd = use chan >>= (\c -> liftIO $ writeBChan c cmd)
