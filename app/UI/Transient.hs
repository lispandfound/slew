{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Transient (
    -- * Types
    TransientPrefix (..),
    TransientState,
    TransientMsg (..),

    -- * Lenses
    char,
    style,
    name,
    command,

    -- * Construction
    leaf,
    node,
    menu,
    item,
    submenu,

    -- * Drawing
    drawTransientView,

    -- * Event handling
    handleTransientEvent,
) where

import Brick (
    AttrName,
    EventM,
    Widget,
    hBox,
    padLeftRight,
    str,
    txt,
    vBox,
    withAttr,
    (<+>),
 )
import Brick.Widgets.Border (hBorderWithLabel)
import Control.Lens (makeLenses, view, (^.))
import Data.Tree (Forest, Tree (Node, rootLabel, subForest))
import Data.Tree.Zipper (
    Full,
    TreePos,
    firstChild,
    fromTree,
    isRoot,
    label,
    next,
    parent,
    tree,
 )
import qualified Graphics.Vty as V

data TransientPrefix m = TransientPrefix
    { _char :: Char
    , _style :: AttrName
    , _name :: Text
    , _command :: Maybe m
    }
    deriving (Show)

-- Builder for convenient construction

newtype TransientBuilder m = TransientBuilder {_unBuilder :: Forest (TransientPrefix m)} deriving (Monoid, Semigroup)

makeLenses ''TransientPrefix
type TransientState m = TreePos Full (TransientPrefix m)

-- | Create a leaf node (action item)
leaf :: Char -> AttrName -> Text -> m -> TransientBuilder m
leaf c attr txt' cmd = TransientBuilder [Node (TransientPrefix c attr txt' (Just cmd)) []]

-- | Create a node with children (submenu)
node :: Char -> AttrName -> Text -> TransientBuilder m -> TransientBuilder m
node c attr txt' (TransientBuilder children) =
    TransientBuilder [Node (TransientPrefix c attr txt' Nothing) children]

-- | Run the builder to create a transient state
menu :: Text -> TransientBuilder m -> TransientState m
menu rootName (TransientBuilder children) =
    fromTree $ Node (TransientPrefix ' ' mempty rootName Nothing) children

-- Convenience functions for common patterns

-- | Create a simple action item
item :: Char -> Text -> m -> TransientBuilder m
item c txt' cmd = leaf c mempty txt' cmd

-- | Create a submenu
submenu :: Char -> Text -> TransientBuilder m -> TransientBuilder m
submenu c txt' = node c mempty txt'

drawTransientView :: TransientState m -> Widget n
drawTransientView menu' = go (tree menu')
  where
    childLabel menu'' = withAttr (menu'' ^. style) (str [menu'' ^. char, ':']) <+> padLeftRight 1 (txt (menu'' ^. name))
    go current =
        vBox
            [ hBorderWithLabel (txt $ (rootLabel current) ^. name)
            , hBox (map (childLabel . rootLabel) $ subForest current)
            ]

findChild :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findChild p pos = firstChild pos >>= go
  where
    go tp
        | p (label tp) = Just tp
        | otherwise = next tp >>= go

-- | TransientMsg is either: close dialog, emit msg, traverse up or traverse down (for submenus).
data TransientMsg m = Close | Msg m | Up | Next

handleTransientEvent :: V.Event -> EventM n (TransientState m) (First (TransientMsg m))
handleTransientEvent (V.EvKey V.KEsc []) = gets shouldClose <* modify goUp
  where
    shouldClose st = if isRoot st then pure Close else pure Up
    goUp = fromMaybe <*> parent
handleTransientEvent (V.EvKey (V.KChar 'g') [V.MCtrl]) = pure (pure Close)
handleTransientEvent (V.EvKey (V.KChar c) []) = do
    nextMenu <- gets (findChild ((== c) . view char))
    case nextMenu of
        Just child -> put child >> gets (First . (<|> pure Next) . fmap Msg . view command . label)
        Nothing -> pure mempty
handleTransientEvent _ = pure mempty
