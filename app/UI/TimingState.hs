module UI.TimingState (
    handleTick,
    handleUpdateTime,
) where

import Brick (EventM)
import Data.Time.Clock.System (getSystemTime)
import Model.TimingState (TimingState (..))
import Optics.State.Operators ((.=))

-- | Handle a tick event by updating the current time
handleTick :: EventM n TimingState ()
handleTick = do
    sysTime <- liftIO getSystemTime
    #currentTime .= sysTime

-- | Update the last update time
handleUpdateTime :: EventM n TimingState ()
handleUpdateTime = do
    curTime <- liftIO getSystemTime
    #lastUpdate .= pure curTime
