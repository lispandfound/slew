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
import Brick.Widgets.Edit (Editor (editContents), editor, handleEditorEvent, renderEditor)
import Brick.Widgets.List (GenericList (listElements), listReplace, listSelected, listSelectedElement)
import Brick.Widgets.TabularList.Mixed (
    AvailWidth (AvlW),
    ColHdrHeight (ColHdrH),
    ColWidth (..),
    Index (Ix),
    ListFocused (..),
    ListItemHeight (LstItmH),
    MixedColCtxt (MColC),
    MixedColHdr (..),
    MixedCtxt (MxdCtxt),
    MixedRenderers (..),
    MixedTabularList (list),
    WidthsPerRow (..),
    WidthsPerRowKind (..),
    handleMixedListEvent,
    mixedTabularList,
    renderMixedTabularList,
 )

import qualified Data.Text as T
import qualified Graphics.Vty as V
import Optics.Label ()
import Optics.Operators ((^.))
import Optics.State (use)
import Optics.State.Operators ((%=), (.=))

import Data.Text.Zipper (getText)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime))
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Fmt (fmt, (+|), (|+))
import Model.Job (
    Job (..),
    Quantity (Set),
    formatTime,
 )
import Optics.Core ((%))
import Optics.Getter (view)
import UI.Themes (column, jobId, jobState, selectedRow, squeue)

type JobTabularList n = MixedTabularList n Job Widths
type JobRenderers n = MixedRenderers n Job Widths

data Widths = Widths {jobCols :: [ColWidth]} deriving (Generic)

data JobQueueState n = JobQueueState
    { searchEditor :: Editor Text n
    , sortKey :: Maybe (Job -> Job -> Ordering)
    , jobListState :: JobTabularList n
    , allJobs :: [Job]
    }
    deriving (Generic)

colHeader :: MixedColHdr n Widths
colHeader =
    MixedColHdr
        { draw = \_ (MColC (Ix ci)) ->
            case columnHeaderNames Vec.!? ci of
                Just colName -> withAttr (squeue <> column <> attrName (toString colName)) (padRight Max (txt colName) <+> str " ") <=> hBorder
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
        { searchEditor = editor editName (Just 1) ""
        , jobListState = mixedTabularList listName mempty (LstItmH 1) columnWidths widthsPerRow
        , allJobs = []
        , sortKey = Nothing
        }

filterJobs :: Text -> [Job] -> [Job]
filterJobs "" = id
filterJobs searchTerm =
    let searchLower = T.toLower searchTerm
        matches :: Job -> Bool
        matches job =
            let fields =
                    [ T.pack (show $ job ^. #jobId)
                    , job ^. #name
                    , job ^. #account
                    ]
                        <> job ^. #jobState
                        <> [job ^. #partition]
             in any (searchLower `T.isInfixOf`) $ map T.toLower fields
     in filter matches

updateSortKey :: (Ord n, Show n) => (Job -> Job -> Ordering) -> EventM n (JobQueueState n) ()
updateSortKey key = #sortKey .= Just key >> updateListState

updateJobList :: (Ord n, Show n) => [Job] -> EventM n (JobQueueState n) ()
updateJobList jobs = #allJobs .= jobs >> updateListState

updateListState :: (Ord n, Show n) => EventM n (JobQueueState n) ()
updateListState = do
    key <- (fmap sortBy) <$> use #sortKey
    searchTerm <- gets currentSearchTerm
    jobs <- use #allJobs
    selection <- listSelected <$> use (#jobListState % #list)
    #jobListState % #list %= listReplace (filterAndSort searchTerm key jobs) selection
  where
    filterAndSort :: Text -> (Maybe ([Job] -> [Job])) -> [Job] -> Seq Job
    filterAndSort searchTerm key = fromList . fromMaybe id key . filterJobs searchTerm

currentSearchTerm :: JobQueueState n -> Text
currentSearchTerm = mconcat . getText . editContents . view (#searchEditor)

selectedJob :: JobQueueState n -> Maybe Job
selectedJob = fmap snd . listSelectedElement . view (#jobListState % #list)

-- | Render the search bar.
drawSearchBar :: (Ord n, Show n) => JobQueueState n -> Widget n
drawSearchBar st =
    str "Search Jobs: " <+> renderEditor (txt . T.unlines) True (st ^. #searchEditor)

-- | Render the job list widget.
drawJobList :: (Ord n, Show n) => SystemTime -> JobQueueState n -> Widget n
drawJobList currentTime st =
    borderWithLabel (txt . fmt $ "SLURM Queue (" +| length jobs |+ " jobs)") $
        renderMixedTabularList
            (defJobRenderers currentTime)
            (LstFcs True)
            (st ^. #jobListState)
  where
    jobs = listElements (st ^. #jobListState ^. #list)

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
            0 -> withAttr jobId . render . show $ job ^. #jobId
            1 -> render $ job ^. #name
            2 -> render $ job ^. #userName
            3 -> render $ job ^. #account
            4 -> styleAttribute . padRight Max . foldr (\st w -> (jobStateLabel st) <+> w) emptyWidget $ job ^. #jobState
            5 ->
                renderRunningTime
                    (job ^. #startTime)
                    currentTime
            6 -> render $ (nodesFor job)
            _ -> emptyWidget
  where
    nodesFor :: Job -> Text
    nodesFor job' = if T.null (job' ^. #nodes) then "-" else job' ^. #nodes
    styleAttribute = if isFocused then withAttr selectedRow else id
    renderRunningTime (Set t) t'
        | currentTime > t = (render . formatTime) (systemDiffTime t t')
        | otherwise = render "-"
    renderRunningTime _ _ = render "-"
    render s = styleAttribute $ padRight Max (txt s) <+> str " "
    jobStateLabel st =
        if isFocused
            then padRight (Pad 1) (txt st)
            else
                withAttr
                    (jobState <> (attrName (toString st)))
                    (padRight Max (txt st))

columnHeaderNames :: Vector Text
columnHeaderNames = Vec.fromList ["ID", "Name", "User", "Account", "State", "Time", "Nodes"]

columnHeaders :: MixedColHdr n Widths
columnHeaders =
    MixedColHdr
        { draw = \_ (MColC (Ix ci)) -> maybe emptyWidget (padRight Max . txt) (columnHeaderNames Vec.!? ci) <=> hBorder
        , widths = \Widths{jobCols} -> jobCols
        , height = ColHdrH 2
        }

handleJobQueueEvent :: (Ord n, Show n) => V.Event -> EventM n (JobQueueState n) ()
handleJobQueueEvent e@(V.EvKey V.KUp []) = zoom #jobListState (handleMixedListEvent e)
handleJobQueueEvent e@(V.EvKey V.KDown []) = zoom #jobListState (handleMixedListEvent e)
handleJobQueueEvent e = zoom #searchEditor (handleEditorEvent (VtyEvent e)) >> updateListState
