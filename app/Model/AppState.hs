{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppState (
    AppState (..),
    SlewEvent (..),
    Name (..),
    Command (..),
    Category (..),
    searchEditor,
    jobList,
    allJobs,
    selectedJob,
    sortKey,
    transient,
    initialState,
    getCurrentSearchTerm,
    getSelectedJob,
    filterJobs,
    updateJobList,
) where

import Brick.Widgets.Edit (Editor, editor, getEditContents)
import Brick.Widgets.List (GenericList, list, listReplace, listSelectedElement)
import Control.Lens
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Model.Job
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show)
data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data SlewEvent = SQueueStatus [Job] | SControl Command | SortBy Category deriving (Show)

------------------------------------------------------------
-- Widget Names

data Name = SearchEditor | JobListWidget
    deriving (Eq, Ord, Show)

------------------------------------------------------------
-- App State

data AppState = AppState
    { _searchEditor :: Editor Text Name
    , _jobList :: GenericList Name Vec.Vector Job
    , _allJobs :: [Job]
    , _selectedJob :: Maybe Job
    , _transient :: Maybe (TR.TransientState SlewEvent)
    , _sortKey :: Maybe Category
    }
    deriving (Show)

makeLenses ''AppState

------------------------------------------------------------
-- Initial State

initialState :: AppState
initialState =
    AppState
        { _searchEditor = editor SearchEditor (Just 1) ""
        , _jobList = list JobListWidget Vec.empty 1
        , _allJobs = []
        , _selectedJob = Nothing
        , _transient = Nothing
        , _sortKey = Nothing
        }

------------------------------------------------------------
-- Derived Accessors

getCurrentSearchTerm :: AppState -> Text
getCurrentSearchTerm = mconcat . getEditContents . view searchEditor

getSelectedJob :: AppState -> Maybe Job
getSelectedJob = fmap snd . listSelectedElement . view jobList

------------------------------------------------------------
-- Pure Updates

filterJobs :: Text -> [Job] -> [Job]
filterJobs "" = id
filterJobs searchTerm =
    let searchLower = T.toLower searchTerm
        matches job =
            let fields =
                    [ T.pack (show $ jobId job)
                    , name job
                    , account job
                    ]
                        <> jobState job
                        <> [partition job]
             in any (searchLower `T.isInfixOf`) $ map T.toLower fields
     in filter matches

updateJobList :: Text -> AppState -> AppState
updateJobList term st =
    let filtered = filterJobs term (st ^. allJobs)
        newList = listReplace (Vec.fromList filtered) (Just 0) (st ^. jobList)
     in st & jobList .~ newList
