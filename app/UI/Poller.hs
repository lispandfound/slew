{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Poller (PollerState, PollEvent (..), drawPoller, poller, handlePollerEvent, currentFile, tailFile, startPoller, commandChannel, buffer, maxLines) where

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
    , _maxLines :: Int
    , _currentFile :: Maybe Text
    }

makeLenses ''PollerState

data PollEvent = PollError String | PollUpdate (Seq Text) deriving (Show)

poller :: TChan Command -> Int -> PollerState
poller chan maxLines' =
    PollerState
        { _commandChannel = chan
        , _buffer = Right mempty
        , _maxLines = maxLines'
        , _currentFile = Nothing
        }

takeR :: Int -> Seq Text -> Seq Text
takeR len seq' = Seq.drop (max 0 (Seq.length seq' - len)) seq'

startPoller :: TChan Command -> (PollEvent -> IO ()) -> IO ()
startPoller chan f = createPoller chan handle (pure ())
  where
    handle event chunk = f (decode event chunk)
    decode (Left err) = const (PollError err)
    decode _ = either (PollError . show) (PollUpdate . fromList . T.lines) . decodeUtf8'

tailFile :: FilePath -> EventM n PollerState ()
tailFile fp = do
    chan <- use commandChannel
    liftIO . atomically $ writeTChan chan (Tail fp)
    buffer .= Right mempty
    currentFile .= Just (toText fp)

handlePollerEvent :: PollEvent -> EventM n PollerState ()
handlePollerEvent (PollError err) = buffer .= (Left err)
handlePollerEvent (PollUpdate buf) = do
    currentBuffer <- use buffer
    allowedLength <- use maxLines
    buffer .= Right (takeR allowedLength (fromRight mempty currentBuffer <> buf))

pollTextWidget :: PollerState -> Widget n
pollTextWidget (PollerState{_buffer = (Right lns)}) = txt . foldMap (<> "\n") $ lns
pollTextWidget (PollerState{_buffer = (Left err)}) = str err

drawPoller :: PollerState -> Widget n
drawPoller st =
    case st ^. currentFile of
        Nothing -> emptyWidget
        Just fp ->
            borderWithLabel (txt fp) . padRight Max $ pollTextWidget st
