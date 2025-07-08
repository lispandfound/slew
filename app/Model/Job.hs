{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Job (
    ExitCode,
    Job,
    account,
    cpus,
    endTime,
    exitCode,
    jobId,
    jobState,
    makeLenses,
    memoryPerNode,
    nodes,
    name,
    nodeCount,
    partition,
    returnCode,
    standardError,
    standardOutput,
    startTime,
    stateReason,
    status,
    timeLimit,
    userName,
    showWith,
    formatTime,
) where

import Control.Lens (makeLenses)
import Data.Aeson (
    FromJSON (parseJSON),
    Options (fieldLabelModifier),
    defaultOptions,
    genericParseJSON,
    withObject,
    (.:),
 )
import Data.Aeson.Casing (snakeCase)
import Data.Time.Clock (DiffTime)
import Data.Time.Clock.System (SystemTime)
import Fmt (padLeftF, (+|), (|+))

data Quantity a = Unset | Infinite | Set a deriving (Show, Eq)

instance (Ord a) => Ord (Quantity a) where
    compare Unset Unset = EQ
    compare Infinite Infinite = EQ
    compare Unset _ = LT
    compare _ Unset = GT
    compare Infinite _ = GT
    compare _ Infinite = LT
    compare (Set x) (Set y) = compare x y

showWith :: (IsString b) => (a -> b) -> Quantity a -> b
showWith _ Unset = "not set"
showWith _ Infinite = "infinite"
showWith f (Set x) = f x

snakeCaseOptions :: Options
snakeCaseOptions = defaultOptions{fieldLabelModifier = (snakeCase . fromMaybe mempty . viaNonEmpty tail)}

instance (FromJSON a) => FromJSON (Quantity a) where
    parseJSON = withObject "Quantity" $ \obj -> do
        isSet <- obj .: "set"
        infinite <- obj .: "infinite"
        number <- obj .: "number"
        case (isSet, infinite, number) of
            (False, _, _) -> pure Unset
            (True, True, _) -> pure Infinite
            (True, False, x) -> pure (Set x)

data ExitCode = ExitCode
    { _status :: [Text]
    , _returnCode :: Quantity Int
    }
    deriving (Show, Generic)

makeLenses ''ExitCode

instance FromJSON ExitCode where
    parseJSON = genericParseJSON snakeCaseOptions

data Job = Job
    { _account :: Text
    , _cpus :: Quantity Int
    , _endTime :: Quantity SystemTime
    , _exitCode :: ExitCode
    , _jobId :: Int
    , _jobState :: [Text]
    , _memoryPerNode :: Quantity Int
    , _name :: Text
    , _nodeCount :: Quantity Int
    , _standardOutput :: FilePath
    , _standardError :: FilePath
    , _nodes :: Text
    , _partition :: Text
    , _startTime :: Quantity SystemTime
    , _stateReason :: Text
    , _timeLimit :: Quantity DiffTime
    , _userName :: Text
    }
    deriving (Show, Generic)

makeLenses ''Job

instance FromJSON Job where
    parseJSON = genericParseJSON snakeCaseOptions

-- | Format seconds to HH:MM:SS string
formatTime :: DiffTime -> Text
formatTime diff =
    let
        seconds = round diff :: Int
        hours = seconds `div` 3600
        minutes = (seconds `mod` 3600) `div` 60
        secs = seconds `mod` 60
     in
        "" +| padLeftF 2 '0' hours |+ ":" +| padLeftF 2 '0' minutes |+ ":" +| padLeftF 2 '0' secs |+ ""
