{-# LANGUAGE TemplateHaskell #-}
module UI.Transient where

import Brick hiding (style)
import Control.Lens hiding (children)
import qualified Graphics.Vty as V
import Brick.Widgets.Border ( hBorderWithLabel )
import Data.Tree.Zipper
import Data.Tree



data TransientPrefix m = TransientPrefix {
  _char :: Char
  , _style :: AttrName
  , _name :: Text
  , _command :: Maybe m
  }
makeLenses ''TransientPrefix
type TransientState m = TreePos Full (TransientPrefix m)

renderTransientView :: TransientState e -> Widget n
renderTransientView menu = go (tree menu)
    where childLabel menu = withAttr (menu ^. style) (str [menu ^. char, ':']) <+> txt (menu ^. name)
          go current = vBox [
            hBorderWithLabel (txt $ (rootLabel current) ^. name)
            , hBox (map (childLabel . rootLabel) $ subForest current)
            ]


findChild :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findChild p pos = firstChild pos >>= go
  where
    go tp
      | p (label tp) = Just tp
      | otherwise    = next tp >>= go


handleTransientEvent :: V.Event -> EventM n (Maybe (TransientState m)) ()
handleTransientEvent (V.EvKey V.KEsc []) = modify (>>= parent)
handleTransientEvent (V.EvKey (V.KChar c) []) = modify (>>= (findChild ((== c) . view char)))
handleTransientEvent _ = pure ()
