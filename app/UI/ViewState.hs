module UI.ViewState (
    handleViewPush,
    handleViewPop,
    handleViewPopWithHalt,
) where

import Brick (EventM, halt)
import Model.ViewState (View, ViewState, isSingleton, popView, pushView)

-- | Push a view onto the view stack
handleViewPush :: View -> EventM n ViewState ()
handleViewPush view = modify (pushView view)

-- | Pop a view from the view stack
handleViewPop :: EventM n ViewState ()
handleViewPop = modify popView

-- | Pop a view from the view stack, halting if only one view remains
handleViewPopWithHalt :: EventM n ViewState ()
handleViewPopWithHalt = do
    shouldHalt <- gets isSingleton
    when shouldHalt halt
    handleViewPop
