{-# LANGUAGE TemplateHaskell #-}

module UI.Poller (renderPoller, handlePollerEvent, tailFile, createOutputPoller) where

import Brick
import Brick.BChan
import Brick.Widgets.Border (hBorderWithLabel)
import Control.Concurrent.STM.TChan
import Control.Lens
import qualified Data.Text as T
import Tail.Poller (Command (..), createPoller)

data PollerState = PollerState
    { _commandChannel :: TChan Command
    , _buffer :: Either (Seq Text) Text
    , _maxLines :: Int
    }

makeLenses ''PollerState

data PollEvent = PollError String | PollUpdate (Seq Text)

createOutputPoller :: TChan Command -> (PollEvent -> IO ()) -> IO ()
createOutputPoller commandChannel f = createPoller commandChannel handle (pure ())
  where
    handle event chunk = f (decode event chunk)
    decode (Left err) = const (PollError err)
    decode _ = either (PollError . show) (PollUpdate . fromList . T.lines) . decodeUtf8'

tailFile :: FilePath -> EventM n PollerState ()
tailFile fp = do
    chan <- use commandChannel
    liftIO . atomically $ writeTChan chan (Tail fp)
    tailLines .= mempty
