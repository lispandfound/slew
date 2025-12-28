module UI.TimingState (
    handleTick,
    handleUpdateTime,
) where

import Brick (EventM, get, put)
import Data.Time.Clock.System (getSystemTime)
import Model.TimingState (TimingState, updateCurrentTime, updateLastUpdate)

-- | Handle a tick event by updating the current time
handleTick :: EventM n TimingState ()
handleTick = do
    sysTime <- liftIO getSystemTime
    get >>= put . updateCurrentTime sysTime

-- | Update the last update time
handleUpdateTime :: EventM n TimingState ()
handleUpdateTime = do
    curTime <- liftIO getSystemTime
    get >>= put . updateLastUpdate (pure curTime)
