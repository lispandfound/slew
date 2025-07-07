{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Poller (renderPoller, handlePollerEvent, tailFile, createOutputPoller, commandChannel, buffer, name, maxLines) where

import Brick
import Brick.Widgets.Border (borderWithLabel)
import Control.Concurrent.STM.TChan
import Control.Lens
import qualified Data.Sequence as Seq

import qualified Data.Text as T
import Tail.Poller (Command (..), createPoller)

data PollerState = PollerState
    { _commandChannel :: TChan Command
    , _buffer :: Either String (Seq Text)
    , _name :: Text
    , _maxLines :: Int
    }

makeLenses ''PollerState

data PollEvent = PollError String | PollUpdate (Seq Text)

takeR :: Int -> Seq Text -> Seq Text
takeR len = Seq.reverse . Seq.take len . Seq.reverse

createOutputPoller :: TChan Command -> (PollEvent -> IO ()) -> IO ()
createOutputPoller chan f = createPoller chan handle (pure ())
  where
    handle event chunk = f (decode event chunk)
    decode (Left err) = const (PollError err)
    decode _ = either (PollError . show) (PollUpdate . fromList . T.lines) . decodeUtf8'

tailFile :: FilePath -> EventM n PollerState ()
tailFile fp = do
    chan <- use commandChannel
    liftIO . atomically $ writeTChan chan (Tail fp)
    buffer .= Right mempty

handlePollerEvent :: PollEvent -> EventM n PollerState ()
handlePollerEvent (PollError err) = buffer .= (Left err)
handlePollerEvent (PollUpdate buf) = do
    currentBuffer <- use buffer
    allowedLength <- use maxLines
    buffer .= Right (takeR allowedLength (fromRight mempty currentBuffer <> buf))

renderPoller :: PollerState -> Widget n
renderPoller (PollerState{_name = n, _buffer = (Right lns)}) = borderWithLabel (txt n) . txt . foldMap (<> "\n") $ lns
renderPoller (PollerState{_name = n, _buffer = (Left err)}) = (borderWithLabel (txt n) . str) err
