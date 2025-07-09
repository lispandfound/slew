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
    padLeft,
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
    to,
    use,
    (%=),
    (.=),
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

data JobQueueState n = JobQueueState
    { _searchEditor :: Editor Text n
    , _sortKey :: Maybe (Job -> Job -> Ordering)
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

updateSortKey :: (Ord n, Show n) => (Job -> Job -> Ordering) -> EventM n (JobQueueState n) ()
updateSortKey key = sortKey .= Just key >> updateListState

updateJobList :: (Ord n, Show n) => [Job] -> EventM n (JobQueueState n) ()
updateJobList jobs = allJobs .= jobs >> updateListState

updateListState :: (Ord n, Show n) => EventM n (JobQueueState n) ()
updateListState = do
    key <- use (sortKey . to (fmap sortBy))
    searchTerm <- use currentSearchTerm
    jobs <- use allJobs
    selection <- use (jobListState . listSelectedL)
    jobListState %= listReplace ((Vec.fromList . fromMaybe id key . filterJobs searchTerm) jobs) selection

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

-- | Render the job list widget.
drawJobList :: (Ord n, Show n) => JobQueueState n -> Widget n
drawJobList st =
    borderWithLabel (txt . fmt $ "SLURM Queue (" +| Vec.length (st ^. jobListState ^. listElementsL) |+ " jobs)") $
        renderList
            drawJobItem
            True
            (st ^. jobListState)

-- | Render a single item in the job list.
drawJobItem :: Bool -> Job -> Widget n
drawJobItem selected job =
    let style = if selected then withAttr (attrName "selected") else id
     in style . padRight Max . vBox $
            [ hBox
                [ padLeft (Pad 2) . padRight Max . str . show $ job ^. jobId
                , padRight Max . txt . T.take 12 $ job ^. name
                , padRight Max . txt . T.take 8 $ job ^. account
                , padRight Max . txt . T.take 10 . T.intercalate " " $ job ^. jobState
                , padRight Max . txt . T.take 10 . showWith formatTime $ job ^. timeLimit
                , padRight (Pad 2) . txt $ job ^. nodes
                ]
            ]

handleJobQueueEvent :: (Ord n, Show n) => V.Event -> EventM n (JobQueueState n) ()
handleJobQueueEvent e@(V.EvKey V.KUp []) = zoom jobListState (handleListEvent e)
handleJobQueueEvent e@(V.EvKey V.KDown []) = zoom jobListState (handleListEvent e)
handleJobQueueEvent e = zoom searchEditor (handleEditorEvent (VtyEvent e)) >> updateListState
