module Model.TimingState (
    TimingState (..),
    initialTimingState,
    updateCurrentTime,
    updateLastUpdate,
) where

import Data.Time.Clock.System (SystemTime, getSystemTime)
import Optics.Label ()

data TimingState = TimingState
    { currentTime :: SystemTime
    , lastUpdate :: Maybe SystemTime
    }
    deriving (Generic, Show)

initialTimingState :: IO TimingState
initialTimingState = do
    now <- getSystemTime
    return $ TimingState now Nothing

updateCurrentTime :: SystemTime -> TimingState -> TimingState
updateCurrentTime time st = st{currentTime = time}

updateLastUpdate :: Maybe SystemTime -> TimingState -> TimingState
updateLastUpdate time st = st{lastUpdate = time}
