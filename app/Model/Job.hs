{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Model.Job where

import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Time.Clock.System (SystemTime)
import Data.Time.Clock (DiffTime)

data Quantity a = Unset | Infinite | Set a deriving (Show, Eq)

showWith :: IsString b => (a -> b) -> Quantity a -> b
showWith _ Unset = "not set"
showWith _ Infinite = "infinite"
showWith f (Set x) = f x

snakeCaseOptions :: Options
snakeCaseOptions = defaultOptions { fieldLabelModifier = snakeCase }

instance FromJSON a => FromJSON (Quantity a) where
  parseJSON = withObject "Quantity" $ \obj -> do
    isSet <- obj .: "set"
    infinite <- obj .: "infinite"
    number <- obj .: "number"
    case (isSet, infinite, number) of
      (False, _, _) -> pure Unset
      (True, True, _) -> pure Infinite
      (True, False, x) -> pure (Set x)

data ExitCode = ExitCode {
  status :: [Text]
  , returnCode :: Quantity Int
                         } deriving (Show, Generic)

instance FromJSON ExitCode where
  parseJSON = genericParseJSON snakeCaseOptions

data Job = Job
  { account :: Text
  , cpus :: Quantity Int
  , endTime :: Quantity SystemTime
  , exitCode :: ExitCode
  , jobId :: Int
  , jobState :: [Text]
  , memoryPerNode :: Quantity Int
  , name :: Text
  , nodeCount :: Quantity Int
  , nodes :: Text
  , partition :: Text
  , startTime :: Quantity SystemTime
  , stateReason :: Text
  , timeLimit :: Quantity DiffTime
  , userName :: Text
  } deriving (Show, Generic)



instance FromJSON Job where
    parseJSON = genericParseJSON snakeCaseOptions
