{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.JobList where

import Brick (
    BrickEvent (VtyEvent),
    EventM,
    Padding (Max, Pad),
    Widget,
    attrName,
    hBox,
    padRight,
    str,
    txt,
    vBox,
    withAttr,
    zoom,
    (<+>),
 )
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Edit (Editor, editContentsL, editor, handleEditorEvent, renderEditor)
import Brick.Widgets.List (GenericList, handleListEvent, list, listElementsL, listReplace, listSelectedElementL, listSelectedL, renderList)
import Control.Lens (
    Lens',
    Traversal',
    lens,
    makeLenses,
    (%~),
    (.~),
    (^.),
 )
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Data.Text.Zipper (getText, textZipper)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Fmt (fmt, (+|), (|+))
import Model.Job (
    Job,
    account,
    formatTime,
    jobId,
    jobState,
    name,
    nodes,
    partition,
    showWith,
    timeLimit,
 )

data Sorter = forall a. (Ord a) => Sorter {unSorter :: (Job -> a)}

sorter :: (Ord a) => (Job -> a) -> Sorter
sorter f = Sorter{unSorter = f}

data JobQueueState n = JobQueueState
    { _searchEditor :: Editor Text n
    , _sortKey :: Maybe Sorter
    , _jobListState :: GenericList n Vector Job
    , _allJobs :: [Job]
    }

makeLenses ''JobQueueState

jobList :: n -> n -> JobQueueState n
jobList editName listName =
    JobQueueState
        { _searchEditor = editor editName (Just 1) ""
        , _jobListState = list listName mempty 1
        , _allJobs = []
        , _sortKey = Nothing
        }

filterJobs :: Text -> [Job] -> [Job]
filterJobs "" = id
filterJobs searchTerm =
    let searchLower = T.toLower searchTerm
        matches job =
            let fields =
                    [ T.pack (show $ job ^. jobId)
                    , job ^. name
                    , job ^. account
                    ]
                        <> job ^. jobState
                        <> [job ^. partition]
             in any (searchLower `T.isInfixOf`) $ map T.toLower fields
     in filter matches

currentSearchTerm :: Lens' (JobQueueState n) Text
currentSearchTerm = searchEditor . editContentsL . lens getter setter
  where
    getter = mconcat . getText
    setter _ txt' = textZipper [txt'] (Just 1)

selectedJob :: Traversal' (JobQueueState n) Job
selectedJob = jobListState . listSelectedElementL

-- | Render the search bar.
drawSearchBar :: (Ord n, Show n) => JobQueueState n -> Widget n
drawSearchBar st =
    str "Search Jobs: " <+> renderEditor (txt . T.unlines) True (st ^. searchEditor)

modifyList :: ([a] -> [a]) -> GenericList n Vector a -> GenericList n Vector a
modifyList f l = listReplace ((Vec.fromList . f . Vec.toList) (l ^. listElementsL)) (l ^. listSelectedL) l

-- | Render the job list widget.
drawJobList :: (Ord n, Show n) => JobQueueState n -> Widget n
drawJobList st =
    let
        sortedList = sortList (st ^. sortKey) (st ^. jobListState)
     in
        borderWithLabel (txt . fmt $ "SLURM Queue (" +| Vec.length (sortedList ^. listElementsL) |+ " jobs)") $
            renderList
                drawJobItem
                True
                sortedList
  where
    sortList Nothing = modifyList (filterJobs (st ^. currentSearchTerm))
    sortList (Just (Sorter{unSorter = f})) = modifyList (sortOn f . filterJobs (st ^. currentSearchTerm))

-- | Render a single item in the job list.
drawJobItem :: Bool -> Job -> Widget n
drawJobItem selected job =
    let style = if selected then withAttr (attrName "selected") else id
     in style . padRight Max . vBox $
            [ hBox
                [ padRight (Pad 2) . str . show $ job ^. jobId
                , padRight (Pad 2) . txt . T.take 12 $ job ^. name
                , padRight (Pad 2) . txt . T.take 8 $ job ^. account
                , padRight (Pad 2) . txt . T.take 10 . T.intercalate " " $ job ^. jobState
                , padRight (Pad 2) . txt . T.take 10 . showWith formatTime $ job ^. timeLimit
                , txt $ job ^. nodes
                ]
            ]

handleJobQueueEvent :: (Ord n, Show n) => V.Event -> EventM n (JobQueueState n) ()
handleJobQueueEvent e@(V.EvKey V.KUp []) = zoom jobListState (handleListEvent e)
handleJobQueueEvent e@(V.EvKey V.KDown []) = zoom jobListState (handleListEvent e)
handleJobQueueEvent e = zoom searchEditor $ handleEditorEvent (VtyEvent e)
