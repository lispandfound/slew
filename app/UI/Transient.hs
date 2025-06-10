{-# LANGUAGE TemplateHaskell #-}
module UI.Transient where

import Brick hiding (style)
import Control.Lens hiding (children)
import qualified Graphics.Vty as V
import Brick.Widgets.Border
import Control.Zipper 

type AppEvent s n = EventM s n ()

data TransientMenu s n = Menu {
    _style :: Maybe AttrName
    , _command :: Maybe (AppEvent s n)
    , _description :: Text
    , _children :: Map Char (TransientMenu s n)
} 

makeLenses ''TransientMenu

type TransientState s n = Zipper (TransientMenu s n) (TransientMenu s n)

renderTransientView :: TransientState s n -> Widget n
renderTransientView menu = vBox [
    hBorderWithLabel (menu ^. description) 
    , hBox (map childLabel $ toList (menu ^. children))
    ]
    where childLabel (key, menu) = withAttr (menu ^. style) (str [key, ':']) <+> txt (menu ^. description)  

handleTransientEvent :: V.Event -> EventM (TransientState s n) n (Maybe (AppEvent s n))
handleTransientEvent = undefined 

