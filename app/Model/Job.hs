{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.Job where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics 
import Control.Applicative ((<|>))
import Model.JobResources hiding (nodes)


-- | Job information
data Job = Job
  { account :: Text
  , accrueTime :: Int
  , adminComment :: Text
  , arrayJobId :: Int
  , arrayTaskId :: Maybe Int
  , arrayMaxTasks :: Int
  , arrayTaskString :: Text
  , associationId :: Int
  , batchFeatures :: Text
  , batchFlag :: Bool
  , batchHost :: Text
  , flags :: [Text]
  , burstBuffer :: Text
  , burstBufferState :: Text
  , cluster :: Text
  , clusterFeatures :: Text
  , command :: Text
  , comment :: Text
  , contiguous :: Bool
  , coreSpec :: Maybe Int
  , threadSpec :: Maybe Int
  , coresPerSocket :: Maybe Int
  , billableTres :: Double
  , cpusPerTask :: Maybe Int
  , cpuFrequencyMinimum :: Maybe Int
  , cpuFrequencyMaximum :: Maybe Int
  , cpuFrequencyGovernor :: Maybe Text
  , cpusPerTres :: Text
  , deadline :: Int
  , delayBoot :: Int
  , dependency :: Text
  , derivedExitCode :: Int
  , eligibleTime :: Int
  , endTime :: Int
  , excludedNodes :: Text
  , exitCode :: Int
  , features :: Text
  , federationOrigin :: Text
  , federationSiblingsActive :: Text
  , federationSiblingsViable :: Text
  , gresDetail :: [Text]
  , groupId :: Int
  , jobId :: Int
  , jobResources :: JobResources
  , jobState :: Text
  , lastSchedEvaluation :: Int
  , licenses :: Text
  , maxCpus :: Int
  , maxNodes :: Int
  , mcsLabel :: Text
  , memoryPerTres :: Text
  , name :: Text
  , nodes :: Text
  , nice :: Maybe Int
  , tasksPerCore :: Maybe Int
  , tasksPerNode :: Int
  , tasksPerSocket :: Maybe Int
  , tasksPerBoard :: Int
  , cpus :: Int
  , nodeCount :: Int
  , tasks :: Int
  , hetJobId :: Int
  , hetJobIdSet :: Text
  , hetJobOffset :: Int
  , partition :: Text
  , memoryPerNode :: Maybe Int
  , memoryPerCpu :: Int
  , minimumCpusPerNode :: Int
  , minimumTmpDiskPerNode :: Int
  , preemptTime :: Int
  , preSusTime :: Int
  , priority :: Integer
  , profile :: Maybe Text
  , qos :: Text
  , reboot :: Bool
  , requiredNodes :: Text
  , requeue :: Bool
  , resizeTime :: Int
  , restartCnt :: Int
  , resvName :: Text
  , shared :: Maybe Bool
  , showFlags :: [Text]
  , socketsPerBoard :: Int
  , socketsPerNode :: Maybe Int
  , startTime :: Int
  , stateDescription :: Text
  , stateReason :: Text
  , standardError :: Text
  , standardInput :: Text
  , standardOutput :: Text
  , submitTime :: Int
  , suspendTime :: Int
  , systemComment :: Text
  , timeLimit :: Int
  , timeMinimum :: Int
  , threadsPerCore :: Maybe Int
  , tresBind :: Text
  , tresFreq :: Text
  , tresPerJob :: Text
  , tresPerNode :: Text
  , tresPerSocket :: Text
  , tresPerTask :: Text
  , tresReqStr :: Text
  , tresAllocStr :: Text
  , userId :: Int
  , userName :: Text
  , wckey :: Text
  , currentWorkingDirectory :: Text
  } deriving (Show, Generic)

instance FromJSON Job where
  parseJSON = withObject "Job" $ \o -> Job
    <$> o .: "account"
    <*> o .: "accrue_time"
    <*> o .: "admin_comment"
    <*> o .: "array_job_id"
    <*> o .: "array_task_id"
    <*> o .: "array_max_tasks"
    <*> o .: "array_task_string"
    <*> o .: "association_id"
    <*> o .: "batch_features"
    <*> o .: "batch_flag"
    <*> o .: "batch_host"
    <*> o .: "flags"
    <*> o .: "burst_buffer"
    <*> o .: "burst_buffer_state"
    <*> o .: "cluster"
    <*> o .: "cluster_features"
    <*> o .: "command"
    <*> o .: "comment"
    <*> o .: "contiguous"
    <*> o .: "core_spec"
    <*> o .: "thread_spec"
    <*> o .: "cores_per_socket"
    <*> o .: "billable_tres"
    <*> o .: "cpus_per_task"
    <*> o .: "cpu_frequency_minimum"
    <*> o .: "cpu_frequency_maximum"
    <*> o .: "cpu_frequency_governor"
    <*> o .: "cpus_per_tres"
    <*> o .: "deadline"
    <*> o .: "delay_boot"
    <*> o .: "dependency"
    <*> o .: "derived_exit_code"
    <*> o .: "eligible_time"
    <*> o .: "end_time"
    <*> o .: "excluded_nodes"
    <*> o .: "exit_code"
    <*> o .: "features"
    <*> o .: "federation_origin"
    <*> o .: "federation_siblings_active"
    <*> o .: "federation_siblings_viable"
    <*> o .: "gres_detail"
    <*> o .: "group_id"
    <*> o .: "job_id"
    <*> o .: "job_resources"
    <*> o .: "job_state"
    <*> o .: "last_sched_evaluation"
    <*> o .: "licenses"
    <*> o .: "max_cpus"
    <*> o .: "max_nodes"
    <*> o .: "mcs_label"
    <*> o .: "memory_per_tres"
    <*> o .: "name"
    <*> o .: "nodes"
    <*> o .: "nice"
    <*> o .: "tasks_per_core"
    <*> o .: "tasks_per_node"
    <*> o .: "tasks_per_socket"
    <*> o .: "tasks_per_board"
    <*> o .: "cpus"
    <*> o .: "node_count"
    <*> o .: "tasks"
    <*> o .: "het_job_id"
    <*> o .: "het_job_id_set"
    <*> o .: "het_job_offset"
    <*> o .: "partition"
    <*> o .: "memory_per_node"
    <*> o .: "memory_per_cpu"
    <*> o .: "minimum_cpus_per_node"
    <*> o .: "minimum_tmp_disk_per_node"
    <*> o .: "preempt_time"
    <*> o .: "pre_sus_time"
    <*> o .: "priority"
    <*> o .: "profile"
    <*> o .: "qos"
    <*> o .: "reboot"
    <*> o .: "required_nodes"
    <*> o .: "requeue"
    <*> o .: "resize_time"
    <*> o .: "restart_cnt"
    <*> o .: "resv_name"
    <*> o .: "shared"
    <*> o .: "show_flags"
    <*> o .: "sockets_per_board"
    <*> o .: "sockets_per_node"
    <*> o .: "start_time"
    <*> o .: "state_description"
    <*> o .: "state_reason"
    <*> o .: "standard_error"
    <*> o .: "standard_input"
    <*> o .: "standard_output"
    <*> o .: "submit_time"
    <*> o .: "suspend_time"
    <*> o .: "system_comment"
    <*> o .: "time_limit"
    <*> o .: "time_minimum"
    <*> o .: "threads_per_core"
    <*> o .: "tres_bind"
    <*> o .: "tres_freq"
    <*> o .: "tres_per_job"
    <*> o .: "tres_per_node"
    <*> o .: "tres_per_socket"
    <*> o .: "tres_per_task"
    <*> o .: "tres_req_str"
    <*> o .: "tres_alloc_str"
    <*> o .: "user_id"
    <*> o .: "user_name"
    <*> o .: "wckey"
    <*> o .: "current_working_directory"


