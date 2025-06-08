{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.JobResources where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics 
import Control.Applicative ((<|>))
import Model.AllocatedNode (AllocatedNode)

-- | Job resources allocation information
data JobResources = JobResources
  { nodes :: Maybe Text
  , allocatedCpus :: Maybe Int
  , allocatedHosts :: Maybe Int
  , allocatedNodes :: Maybe (Map Text AllocatedNode)
  } deriving (Show, Generic)

instance FromJSON JobResources where
  parseJSON = withObject "JobResources" $ \o -> JobResources
    <$> o .:? "nodes"
    <*> o .:? "allocated_cpus"
    <*> o .:? "allocated_hosts"
    <*> o .:? "allocated_nodes"
