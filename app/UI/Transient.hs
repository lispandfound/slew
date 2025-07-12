{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Transient (
    -- * Types
    TransientPrefix (..),
    TransientState,
    TransientMsg (..),

    -- * Construction
    leaf,
    node,
    menu,
    item,
    submenu,
    horizontalLayout,
    verticalLayout,
    verticalLayoutWithLabel,

    -- * Drawing
    drawTransientView,

    -- * Event handling
    handleTransientEvent,
) where

import Brick (
    AttrName,
    EventM,
    Padding (..),
    Widget,
    emptyWidget,
    hBox,
    padLeftRight,
    padRight,
    str,
    txt,
    vBox,
    withAttr,
    (<+>),
 )
import Brick.Widgets.Border (hBorderWithLabel)
import Data.Tree (Forest, Tree (Node, rootLabel))
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
import Optics.Getter (view)
import Optics.Label ()
import Optics.Operators ((^.))
import qualified UI.Themes as Th

data TransientPrefix m n = TransientPrefix
    { char :: Char
    , name :: Text
    , command :: Maybe m
    , widget :: Widget n
    }
    deriving (Generic)

-- Builder for convenient construction

data TransientBuilder m n = TransientBuilder {draw :: Widget n, tree :: Forest (TransientPrefix m n)} deriving (Generic)

type TransientState m n = TreePos Full (TransientPrefix m n)

horizontalLayout :: [TransientBuilder m n] -> TransientBuilder m n
horizontalLayout nodes = TransientBuilder (hBox . map (padRight (Pad 5) . view #draw) $ nodes) (mconcat . map (view #tree) $ nodes)

verticalLayout :: [TransientBuilder m n] -> TransientBuilder m n
verticalLayout = verticalLayoutWithLabel emptyWidget

verticalLayoutWithLabel :: Widget n -> [TransientBuilder m n] -> TransientBuilder m n
verticalLayoutWithLabel label' nodes = TransientBuilder (vBox . (label' :) . map (view #draw) $ nodes) (mconcat . map (view #tree) $ nodes)

childLabel :: AttrName -> Char -> Text -> Widget n
childLabel style char name = withAttr style (str [char, ':']) <+> padLeftRight 1 (txt name)

-- | Create a leaf node (action item)
leaf :: Char -> AttrName -> Text -> m -> TransientBuilder m n
leaf c attr txt' cmd = TransientBuilder (childLabel attr c txt') [Node (TransientPrefix c txt' (Just cmd) emptyWidget) []]

-- | Create a node with children (submenu)
node :: Char -> AttrName -> Text -> TransientBuilder m n -> TransientBuilder m n
node c attr txt' (TransientBuilder draw children) =
    TransientBuilder (childLabel attr c txt') [Node (TransientPrefix c txt' Nothing draw) children]

-- | Run the builder to create a transient state
menu :: Text -> TransientBuilder m n -> TransientState m n
menu rootName (TransientBuilder draw children) =
    fromTree $ Node (TransientPrefix ' ' rootName Nothing draw) children

-- Convenience functions for common patterns

-- | Create a simple action item
item :: Char -> Text -> m -> TransientBuilder m n
item c txt' cmd = leaf c (Th.transient <> Th.label) txt' cmd

-- | Create a submenu
submenu :: Char -> Text -> TransientBuilder m n -> TransientBuilder m n
submenu c txt' = node c (Th.transient <> Th.submenu) txt'

drawTransientView :: TransientState m n -> Widget n
drawTransientView menu' = go (tree menu')
  where
    go :: Tree (TransientPrefix m n) -> Widget n
    go current =
        vBox
            [ hBorderWithLabel (txt $ (rootLabel current) ^. #name)
            , (view #widget . rootLabel) current
            ]

findChild :: (a -> Bool) -> TreePos Full a -> Maybe (TreePos Full a)
findChild p pos = firstChild pos >>= go
  where
    go tp
        | p (label tp) = Just tp
        | otherwise = next tp >>= go

-- | TransientMsg is either: close dialog, emit msg, traverse up or traverse down (for submenus).
data TransientMsg m = Close | Msg m | Up | Next

handleTransientEvent :: V.Event -> EventM n (TransientState m n) (First (TransientMsg m))
handleTransientEvent (V.EvKey V.KEsc []) = gets shouldClose <* modify goUp
  where
    shouldClose st = if isRoot st then pure Close else pure Up
    goUp = fromMaybe <*> parent
handleTransientEvent (V.EvKey (V.KChar 'g') [V.MCtrl]) = pure (pure Close)
handleTransientEvent (V.EvKey (V.KChar c) []) = do
    nextMenu <- gets (findChild ((== c) . view #char))
    case nextMenu of
        Just child -> put child >> gets (First . (<|> pure Next) . fmap Msg . view #command . label)
        Nothing -> pure mempty
handleTransientEvent _ = pure mempty
