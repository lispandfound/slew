{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.SQueue where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics hiding (Meta)
import Control.Applicative ((<|>))
import Model.Job hiding (cpus, nodes)

-- | Main response structure
data SlurmResponse = SlurmResponse
  { meta :: Meta
  , errors :: [Text]
  , jobs :: [Job]
  } deriving (Show, Generic, FromJSON)


-- | Meta information about the response
data Meta = Meta
  { plugin :: Plugin
  , slurm :: SlurmInfo
  } deriving (Show, Generic)

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \o -> Meta
    <$> o .: "plugin"
    <*> o .: "Slurm"


-- | Plugin information
data Plugin = Plugin
  { pluginType :: Text
  , pluginName :: Text
  } deriving (Show, Generic)

instance FromJSON Plugin where
  parseJSON = withObject "Plugin" $ \o -> Plugin
    <$> o .: "type"
    <*> o .: "name"

-- | Slurm version information
data SlurmInfo = SlurmInfo
  { version :: SlurmVersion
  , release :: Text
  } deriving (Show, Generic)

instance FromJSON SlurmInfo where
  parseJSON = withObject "SlurmInfo" $ \o -> SlurmInfo
    <$> o .: "version"
    <*> o .: "release"

-- | Slurm version details
data SlurmVersion = SlurmVersion
  { major :: Int
  , micro :: Int
  , minor :: Int
  } deriving (Show, Generic)

instance FromJSON SlurmVersion


