module Logic.JobFiltering (
    filterJobs,
    sortJobsBy,
    Category (..),
    sortListByCat,
) where

import qualified Data.Text as T
import Model.Job (Job (..))
import Optics.Getter (view)
import Optics.Operators ((^.))

-- | Job categories for sorting
data Category = Account | CPUs | StartTime | EndTime | JobName | UserName | Memory deriving (Show, Eq)

-- | Filter jobs based on search term
-- Searches across job ID, name, account, state, and partition
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

-- | Get a comparator function for a given category
sortListByCat :: Category -> Job -> Job -> Ordering
sortListByCat Account = comparing (view #account)
sortListByCat CPUs = comparing (view #cpus)
sortListByCat StartTime = comparing (view #startTime)
sortListByCat EndTime = comparing (view #endTime)
sortListByCat JobName = comparing (view #name)
sortListByCat UserName = comparing (view #userName)
sortListByCat Memory = comparing (view #memoryPerNode)

-- | Sort jobs by a given category
sortJobsBy :: Category -> [Job] -> [Job]
sortJobsBy category = sortBy (sortListByCat category)
