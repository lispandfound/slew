{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.SControl (startSControlListener, scontrolLog, drawSControlLog, SControlCmd (..), SControlLogState, log, name, SControlLogEntry (..), result, command, chan, sendSControlCommand, logSControlEvent) where

import Brick
import Brick.BChan
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Control.Lens
import Model.SControl

data SControlCmd
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

handleSControlCommand :: SControlCmd -> IO (Either SControlError SControlOutput)
handleSControlCommand (CancelJob ids) = cancelJob ids
handleSControlCommand (RequeueJob jid) = requeueJob jid
handleSControlCommand (RequeueHoldJob jid) = requeueHoldJob jid
handleSControlCommand (HoldJob ids) = holdJob ids
handleSControlCommand (UserHoldJob ids) = userHoldJob ids
handleSControlCommand (ReleaseJob ids) = releaseJob ids
handleSControlCommand (SuspendJob ids) = suspendJob ids
handleSControlCommand (TopJob ids) = topJob ids
handleSControlCommand (NotifyJob jid msg) = notifyJob jid msg
handleSControlCommand (WriteBatchScript jid fname) = writeBatchScript jid fname
handleSControlCommand (WaitJob jid) = waitJob jid
handleSControlCommand (ResumeJob ids) = resumeJob ids

startSControlListener :: BChan SControlCmd -> (SControlCmd -> Either SControlError SControlOutput -> IO ()) -> IO ()
startSControlListener chan callback = forever $ do
    cmd <- readBChan chan
    output <- handleSControlCommand cmd
    callback cmd output

data SControlLogEntry = SControlLogEntry
    { _command :: SControlCmd
    , _result :: Either SControlError SControlOutput
    }
    deriving (Show)

makeLenses ''SControlLogEntry

data SControlLogState n = SControlLogState
    { _log :: [SControlLogEntry]
    , _name :: n
    , _chan :: BChan SControlCmd
    }
makeLenses ''SControlLogState

scontrolLog :: n -> BChan SControlCmd -> SControlLogState n
scontrolLog name' chan' =
    SControlLogState
        { _log = mempty
        , _name = name'
        , _chan = chan'
        }

formatShellCommand :: SControlCmd -> Widget n
formatShellCommand cmd =
    str "scontrol" <+> case cmd of
        CancelJob ids -> format "cancel" ids
        RequeueJob jid -> format "requeue" [jid]
        RequeueHoldJob jid -> format "requeuehold" [jid]
        HoldJob ids -> format "hold" ids
        UserHoldJob ids -> format "userhold" ids
        ReleaseJob ids -> format "release" ids
        ResumeJob ids -> format "resume" ids
        SuspendJob ids -> format "suspend" ids
        TopJob ids -> format "top" ids
        NotifyJob jid msg -> format "notify" [jid] <+> str msg
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

drawEntry :: SControlLogEntry -> Widget n
drawEntry (SControlLogEntry{_command = com, _result = res}) = vBox [commandLine, commandOutput]
  where
    commandLine = hBox [padRight (Pad 4) exitStatus, shellCommand]
    exitStatus = case res of
        Left (SControlError{exitCode = e}) -> withAttr (attrName "exitFailure") (txt . show $ e)
        Right _ -> withAttr (attrName "exitSuccess") (txt "0")
    commandOutput = case res of
        Left (SControlError{stderr = err}) -> str err
        Right (SControlOutput{stdout = out}) -> str out
    shellCommand = formatShellCommand com

drawSControlLog :: (Ord n, Show n) => SControlLogState n -> Widget n
drawSControlLog st = centerLayer $ borderWithLabel (txt "SControl Log") $ (viewport (st ^. name) Vertical . vBox $ map drawEntry (reverse $ st ^. log))

logSControlEvent :: SControlLogEntry -> EventM n (SControlLogState n) ()
logSControlEvent output = log %= (output :)

-- NOTE: viewportScrolling is handled by name in brick so this occurs farther up the call chain in Event.hs

sendSControlCommand :: SControlCmd -> EventM n (SControlLogState n) ()
sendSControlCommand cmd = use chan >>= (\c -> liftIO $ writeBChan c cmd)
