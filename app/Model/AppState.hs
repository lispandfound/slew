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
    selectedJobL,
    pollTitle,
    sortKey,
    transient,
    pollState,
    initialState,
    getCurrentSearchTerm,
    getSelectedJob,
    filterJobs,
    updateJobList,
) where

import Brick.Widgets.Edit (Editor, editor, getEditContents)
import Brick.Widgets.List (GenericList, list, listReplace, listSelectedElement, listSelectedElementL)
import Control.Concurrent.STM.TChan (TChan)
import Control.Lens (makeLenses, view, (.~), (^.))
import Control.Lens.Traversal (Traversal')
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Model.Job (Job (account, jobId, jobState, name, partition))
import qualified Tail.Poller as P
import UI.Poller (PollerState, poller)
import qualified UI.Poller as UP
import qualified UI.Transient as TR

------------------------------------------------------------
-- Event Messages

data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show)
data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show)
data SlewEvent = SQueueStatus [Job] | PollEvent UP.PollEvent | SControl Command | SortBy Category deriving (Show)

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
    , _pollTitle :: Maybe Text
    , _pollState :: PollerState
    }

makeLenses ''AppState

------------------------------------------------------------
-- Initial State

initialState :: TChan P.Command -> AppState
initialState chan =
    AppState
        { _searchEditor = editor SearchEditor (Just 1) ""
        , _jobList = list JobListWidget Vec.empty 1
        , _allJobs = []
        , _selectedJob = Nothing
        , _transient = Nothing
        , _sortKey = Nothing
        , _pollTitle = Nothing
        , _pollState = poller chan 10
        }

------------------------------------------------------------
-- Derived Accessors

getCurrentSearchTerm :: AppState -> Text
getCurrentSearchTerm = mconcat . getEditContents . view searchEditor

getSelectedJob :: AppState -> Maybe Job
getSelectedJob = fmap snd . listSelectedElement . view jobList

selectedJobL :: Traversal' AppState Job
selectedJobL = jobList . listSelectedElementL

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
