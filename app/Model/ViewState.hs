module Model.ViewState (
    ViewState (..),
    View (..),
    initialViewState,
    pushView,
    popView,
    currentView,
    isSingleton,
) where

import Data.List.NonEmpty ((<|))
import Optics.Label ()

data View = SQueueView | CommandLogView | NodeView
    deriving (Eq, Show)

newtype ViewState = ViewState
    { viewStack :: NonEmpty View
    }
    deriving (Generic, Show)

initialViewState :: ViewState
initialViewState = ViewState (SQueueView :| [])

pushView :: View -> ViewState -> ViewState
pushView v (ViewState stack) = ViewState (v <| stack)

popView :: ViewState -> ViewState
popView (ViewState stack) = ViewState (tail' stack)
  where
    -- tail' is a tail that does not change singletons to preserve the NonEmpty invariant.
    tail' (x :| []) = x :| []
    tail' (_ :| (y : ys)) = y :| ys

currentView :: ViewState -> View
currentView (ViewState stack) = head stack

isSingleton :: ViewState -> Bool
isSingleton (ViewState stack) = null (tail stack)
