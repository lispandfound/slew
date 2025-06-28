{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.Job where

import qualified FFI.Slurm as C
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics 
import Control.Applicative ((<|>))
import Model.JobResources hiding (nodes)
import Foreign.Marshal.Array
import Foreign.Ptr
import System.Posix.Time (epochTime)

-- | Job information
data Job = Job
    { jobId         :: Int
  , userName      :: Text
  , state         :: Text
  , name          :: Text
  , nodes         :: Text
  , timeUsed      :: Text
  , partition     :: Text
  , stdoutPath    :: FilePath
  , stderrPath    :: FilePath
  , numCpus       :: Int
  , jobPriority   :: Int
  , submissionTime :: Text
  } deriving (Show, Generic)
