module UI.Echo (EchoState (..), drawEchoBuffer, echo, clear, echoState, echoStateWith) where

import Brick (EventM, Widget, emptyWidget, txt, (<=>))
import Brick.Widgets.Border (hBorder)
import Optics.Getter (view)
import Optics.Lens ()
import Optics.State (assign)

newtype EchoState = EchoState
    { echoMessage :: Maybe Text
    }
    deriving (Show, Eq, Generic)

echoState :: EchoState
echoState = EchoState Nothing

echoStateWith :: Text -> EchoState
echoStateWith = EchoState . pure

drawEchoBuffer :: EchoState -> Widget n
drawEchoBuffer = maybe emptyWidget ((hBorder <=>) . txt) . view #echoMessage

echo :: Text -> EventM n EchoState ()
echo = assign #echoMessage . pure

clear :: EventM n EchoState ()
clear = assign #echoMessage Nothing
