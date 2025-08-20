module Model.Job (
    ExitCode (..),
    Job (..),
    Quantity (..),
    showWith,
    formatTime,
) where

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
import Data.Time.Clock.System (SystemTime (MkSystemTime))
import Fmt (padLeftF, (+|), (|+))
import Optics.Label ()
import Optics.Setter (over)

data Quantity a = Unset | Infinite | Set a deriving (Show, Eq, Generic)

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
snakeCaseOptions = defaultOptions{fieldLabelModifier = snakeCase}

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
    { status :: [Text]
    , returnCode :: Quantity Int
    }
    deriving (Show, Generic)

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
    , standardOutput :: FilePath
    , standardError :: FilePath
    , nodes :: Text
    , partition :: Text
    , startTime :: Quantity SystemTime
    , stateReason :: Text
    , timeLimit :: Quantity DiffTime
    , userName :: Text
    }
    deriving (Show, Generic)

normaliseDates :: Job -> Job
normaliseDates job =
    job
        & over
            #startTime
            normalise
        & over
            #endTime
            normalise
  where
    normalise (Set (MkSystemTime 0 0)) = Unset
    normalise x = x

fixPaths :: Job -> Job
fixPaths job = job & over #standardOutput normalisePath & over #standardError normalisePath
  where 
    normalisePath = filter (/= '\\')

instance FromJSON Job where
    parseJSON = fmap fixPaths . fmap normaliseDates . genericParseJSON snakeCaseOptions

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
