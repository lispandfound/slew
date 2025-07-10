{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.JobList where

import Brick (
    BrickEvent (VtyEvent),
    EventM,
    Padding (Max, Pad),
    Widget,
    attrName,
    emptyWidget,
    padRight,
    str,
    txt,
    withAttr,
    zoom,
    (<+>),
    (<=>),
 )
import Brick.Widgets.Border (borderWithLabel, hBorder)
import Brick.Widgets.Edit (Editor, editContentsL, editor, handleEditorEvent, renderEditor)
import Brick.Widgets.List (listElementsL, listReplace, listSelectedElementL, listSelectedL)
import Brick.Widgets.TabularList.Mixed
import Control.Lens (
    Ixed (ix),
    Lens',
    Traversal',
    lens,
    makeLenses,
    to,
    use,
    (%=),
    (.=),
    (^.),
    (^?),
 )
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Data.Text.Zipper (getText, textZipper)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime))
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Fmt (fmt, (+|), (|+))
import Model.Job (
    Job,
    Quantity (Set),
    account,
    formatTime,
    jobId,
    jobState,
    name,
    nodes,
    partition,
    startTime,
 )

type JobTabularList n = MixedTabularList n Job Widths
type JobRenderers n = MixedRenderers n Job Widths

data Widths = Widths {jobCols :: [ColWidth]} deriving (Generic)

data JobQueueState n = JobQueueState
    { _searchEditor :: Editor Text n
    , _sortKey :: Maybe (Job -> Job -> Ordering)
    , _jobListState :: JobTabularList n
    , _allJobs :: [Job]
    }

makeLenses ''JobQueueState

colHeader :: MixedColHdr n Widths
colHeader =
    MixedColHdr
        { draw = \_ (MColC (Ix ci)) ->
            case columnHeaderNames Vec.!? ci of
                Just colName -> withAttr (attrName "columnHeader") (padRight Max (txt colName) <+> str " ") <=> hBorder
                Nothing -> emptyWidget
        , widths = \Widths{jobCols} -> jobCols
        , height = ColHdrH 2
        }
defJobRenderers :: SystemTime -> JobRenderers n
defJobRenderers currentTime =
    MixedRenderers
        { cell = (cellRenderer currentTime) -- from earlier
        , rowHdr = Nothing
        , colHdr = Just colHeader -- optional, or Nothing
        , colHdrRowHdr = Nothing
        }

jobList :: n -> n -> JobQueueState n
jobList editName listName =
    JobQueueState
        { _searchEditor = editor editName (Just 1) ""
        , _jobListState = mixedTabularList listName mempty (LstItmH 1) columnWidths widthsPerRow
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
    selection <- use (jobListState . #list . listSelectedL)
    jobListState . #list %= listReplace ((fromList . fromMaybe id key . filterJobs searchTerm) jobs) selection

currentSearchTerm :: Lens' (JobQueueState n) Text
currentSearchTerm = searchEditor . editContentsL . lens getter setter
  where
    getter = mconcat . getText
    setter _ txt' = textZipper [txt'] (Just 1)

selectedJob :: Traversal' (JobQueueState n) Job
selectedJob = jobListState . #list . listSelectedElementL

-- | Render the search bar.
drawSearchBar :: (Ord n, Show n) => JobQueueState n -> Widget n
drawSearchBar st =
    str "Search Jobs: " <+> renderEditor (txt . T.unlines) True (st ^. searchEditor)

-- | Render the job list widget.
drawJobList :: (Ord n, Show n) => SystemTime -> JobQueueState n -> Widget n
drawJobList currentTime st =
    borderWithLabel (txt . fmt $ "SLURM Queue (" +| length jobs |+ " jobs)") $
        renderMixedTabularList
            (defJobRenderers currentTime)
            (LstFcs True)
            (st ^. jobListState)
  where
    jobs = (st ^. jobListState ^. #list ^. listElementsL)

columnWidths :: WidthsPerRowKind Job Widths
columnWidths = WsPerRK $ \(AvlW total) _ ->
    let
        idColumn = 8
        (otherColumns, leftover) = (total - idColumn) `quotRem` (Vec.length columnHeaderNames - 1)
        jobCols = fmap ColW ([idColumn] <> replicate (Vec.length columnHeaderNames - 2) otherColumns <> [otherColumns + leftover])
     in
        Widths{jobCols}

widthsPerRow :: WidthsPerRow Job Widths
widthsPerRow = WsPerR $ \Widths{jobCols} _ -> jobCols

systemDiffTime :: SystemTime -> SystemTime -> DiffTime
systemDiffTime (MkSystemTime s1 ns1) (MkSystemTime s2 ns2) = fromIntegral (s2 - s1) + fromIntegral (ns2 - ns1) * 1e-9

cellRenderer :: SystemTime -> ListFocused -> MixedCtxt -> Job -> Widget n
cellRenderer currentTime (LstFcs isFocused) (MxdCtxt _ (MColC (Ix ci))) job =
    let
     in case ci of
            0 -> withAttr (attrName "jobId") . render . show $ job ^. jobId
            1 -> render $ job ^. name
            2 -> render $ job ^. account
            3 -> styleAttribute . padRight Max . foldr (\st w -> (jobStateLabel st) <+> w) emptyWidget $ job ^. jobState
            4 ->
                renderRunningTime
                    (job ^. startTime)
                    currentTime
            5 -> render $ (nodesFor job)
            _ -> emptyWidget
  where
    nodesFor job' = if T.null (job' ^. nodes) then "-" else job' ^. nodes
    styleAttribute = if isFocused then withAttr (attrName "selected") else id
    renderRunningTime (Set t) t'
        | currentTime > t = (render . formatTime) (systemDiffTime t t')
        | otherwise = render "-"
    renderRunningTime _ _ = render "-"
    render s = styleAttribute $ padRight Max (txt s) <+> str " "
    stateStyles :: Map Text String
    stateStyles =
        fromList
            [ ("PENDING", "jobState.PENDING")
            , ("RUNNING", "jobState.RUNNING")
            , ("FAILED", "jobState.FAILED")
            , ("COMPLETED", "jobState.COMPLETED")
            , ("CANCELLED", "jobState.CANCELLED")
            ]
    jobStateLabel st = case stateStyles ^? ix st of
        Just attr | not isFocused -> withAttr (attrName attr) (padRight Max (txt st))
        _ -> padRight (Pad 1) (txt st)

columnHeaderNames :: Vector Text
columnHeaderNames = Vec.fromList ["ID", "Name", "Account", "State", "Time", "Nodes"]

columnHeaders :: MixedColHdr n Widths
columnHeaders =
    MixedColHdr
        { draw = \_ (MColC (Ix ci)) -> maybe emptyWidget (padRight Max . txt) (columnHeaderNames Vec.!? ci) <=> hBorder
        , widths = \Widths{jobCols} -> jobCols
        , height = ColHdrH 2
        }

handleJobQueueEvent :: (Ord n, Show n) => V.Event -> EventM n (JobQueueState n) ()
handleJobQueueEvent e@(V.EvKey V.KUp []) = zoom jobListState (handleMixedListEvent e)
handleJobQueueEvent e@(V.EvKey V.KDown []) = zoom jobListState (handleMixedListEvent e)
handleJobQueueEvent e = zoom searchEditor (handleEditorEvent (VtyEvent e)) >> updateListState
