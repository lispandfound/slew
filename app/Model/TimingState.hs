module Model.TimingState (
    TimingState (..),
    initialTimingState,
) where

import Data.Time.Clock.System (SystemTime, getSystemTime)
import Optics.Label ()

data TimingState = TimingState
    { currentTime :: SystemTime
    , lastUpdate :: Maybe SystemTime
    }
    deriving (Generic, Show)

initialTimingState :: IO TimingState
initialTimingState = TimingState <$> getSystemTime <*> pure Nothing
