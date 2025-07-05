module Tail.Poller (createPoller, Command (..)) where

import Control.Concurrent.STM.TChan
import Control.Concurrent.Async (race)
import Tail.TailFile (tailFile)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

data Command = Tail FilePath | Pause

data State = Tailing FilePath | Idle

waitForever :: IO ()
waitForever = forever (threadDelay maxBound)

createPoller :: TChan Command -> (Either String a -> ByteString -> IO a) -> IO a -> IO ()
createPoller chan callback init = go Idle
  where
    go state = race (action state >> callback (Left "Tail file process failure") mempty) (atomically $ readTChan chan) >>= go . transition
    transition (Left _) = Idle
    transition (Right (Tail fp)) = Tailing fp
    transition (Right Pause) = Idle
    action (Tailing fp) = tailFile fp (callback . Right) init
    action Idle = waitForever
