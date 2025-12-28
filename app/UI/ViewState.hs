module UI.ViewState (
    handleViewPush,
    handleViewPop,
    handleViewPopWithHalt,
) where

import Brick (EventM, halt, get, put)
import Model.ViewState (View, ViewState, isSingleton, popView, pushView)

-- | Push a view onto the view stack
handleViewPush :: View -> EventM n ViewState ()
handleViewPush view = get >>= put . pushView view

-- | Pop a view from the view stack  
handleViewPop :: EventM n ViewState ()
handleViewPop = get >>= put . popView

-- | Pop a view from the view stack, halting if only one view remains
handleViewPopWithHalt :: EventM n ViewState ()
handleViewPopWithHalt = do
    state <- get
    when (isSingleton state) halt
    handleViewPop
