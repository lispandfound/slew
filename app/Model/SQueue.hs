{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.SQueue where

import Data.Aeson (FromJSON (parseJSON), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix)
import Data.Char (toLower)
import Model.Job (Job)

-- | Main response structure
data SlurmResponse = SlurmResponse
    { meta :: Meta
    , errors :: [Text]
    , jobs :: [Job]
    }
    deriving (Show, Generic, FromJSON)

-- | Meta information about the response
data Meta = Meta
    { plugin :: Plugin
    , slurm :: SlurmInfo
    }
    deriving (Show, Generic)

instance FromJSON Meta

-- | Plugin information
data Plugin = Plugin
    { pluginType :: Text
    , pluginName :: Text
    }
    deriving (Show, Generic)

instance FromJSON Plugin where
    parseJSON = genericParseJSON (aesonPrefix (map toLower))

-- | Slurm version information
data SlurmInfo = SlurmInfo
    { version :: SlurmVersion
    , release :: Text
    }
    deriving (Show, Generic)

instance FromJSON SlurmInfo

-- | Slurm version details
data SlurmVersion = SlurmVersion
    { major :: Text
    , micro :: Text
    , minor :: Text
    }
    deriving (Show, Generic)

instance FromJSON SlurmVersion
