{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.AllocatedNode where 

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics 
import Control.Applicative ((<|>))

-- | Individual node allocation details
data AllocatedNode = AllocatedNode
  { sockets :: Map Text Text
  , cores :: Map Text Text
  , memory :: Int
  , cpus :: Int
  } deriving (Show, Generic)

instance FromJSON AllocatedNode where
  parseJSON = withObject "AllocatedNode" $ \o -> AllocatedNode
    <$> o .: "sockets"
    <*> o .: "cores"
    <*> o .: "memory"
    <*> o .: "cpus"

