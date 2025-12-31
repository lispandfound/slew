{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Slurm.Parser (
    parseJobs,
    SlurmDateTime (..),
    monthToByte,
) where

import Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString.Char8 qualified as B8
import Data.Text.Encoding qualified as T
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime))

-- Assuming these are available in your project's scope
import Model.Job (ExitCode (..), Job (..), Quantity (..))

-- | Intermediate structure to hold date parts before year-resolution logic.
data SlurmDateTime = SlurmDateTime
    { sMonth :: Int
    , sDay :: Int
    , sHour :: Int
    , sMinute :: Int
    }
    deriving (Show, Eq)

-- | Main entry point for parsing the Slurm pipe-delimited output.
parseJobs :: ByteString -> Either String [Job]
parseJobs = parseOnly $ do
    -- Skip the header line efficiently
    A.skipWhile (/= '\n') <* endOfLine
    many' (parseJob <* endOfLine)

parseJob :: Parser Job
parseJob = do
    account <- pText <* pipe
    skipFields 3 -- TRES_PER_NODE, MIN_CPUS, MIN_TMP_DISK
    endTimeRaw <- pSlurmTime <* pipe
    skipFields 3 -- FEATURES, GROUP, OVER_SUBSCRIBE
    jobId <- decimal <* pipe
    name <- pText <* pipe
    skipField -- COMMENT
    timeLimit <- pDiffTime <* pipe
    memoryPerNode <- pMemory <* pipe
    skipFields 4 -- REQ_NODES, COMMAND, PRIORITY, QOS
    stateReason <- pText <* pipe
    st <- pText <* pipe
    userName <- pText <* pipe
    skipFields 7 -- RESERVATION through EXEC_HOST
    cpus <- pQuantity decimal <* pipe
    nodeCount <- pQuantity decimal <* pipe
    skipFields 10 -- DEPENDENCY through TIME
    nodes <- pText <* pipe
    skipField -- CONTIGUOUS
    partition <- pText <* pipe
    skipFields 2 -- PRIORITY, NODELIST(REASON)
    startTimeRaw <- pSlurmTime <* pipe
    jobStateRaw <- pText <* pipe
    skipFields 5 -- UID through SCHEDNODES
    _workDir <- pText -- Final field, no pipe

    -- Note: These 'Set' values are placeholders.
    -- You will apply your custom year-logic to startTimeRaw and endTimeRaw here.
    let startTime = Set (MkSystemTime 0 0)
    let endTime = Set (MkSystemTime 0 0)
    let exitCode = ExitCode [st] Unset
    let jobState = [jobStateRaw]
    let standardOutput = ""
    let standardError = ""

    pure Job{..}

-------------------------------------------------------------------------------
-- Optimized Helpers
-------------------------------------------------------------------------------

pipe :: Parser Char
pipe = char '|'

-- | Skip N pipe-delimited fields without decoding or allocating.
skipFields :: Int -> Parser ()
skipFields n = replicateM_ n skipField

skipField :: Parser ()
skipField = A.skipWhile (\c -> c /= '|' && c /= '\n' && c /= '\r') <* pipe

pText :: Parser Text
pText = T.decodeUtf8 <$> A.takeTill (\c -> c == '|' || c == '\n')

pQuantity :: Parser a -> Parser (Quantity a)
pQuantity p =
    (Unset <$ ("N/A" <|> "(null)"))
        <|> (Infinite <$ "infinite")
        <|> (Set <$> p)

-- | Parses memory strings like "1900M" or "2G" into MB.
pMemory :: Parser (Quantity Int)
pMemory = pQuantity $ do
    n <- decimal
    (char 'M' >> pure n) <|> (char 'G' >> pure (n * 1024)) <|> pure n

-- | Parses "Dec 27 12:19" into our intermediate structure.
pSlurmDateTime :: Parser SlurmDateTime
pSlurmDateTime = do
    month <- monthToByte <$> A.take 3
    A.skipSpace
    day <- decimal
    A.skipSpace
    hour <- decimal
    _ <- char ':'
    SlurmDateTime month day hour <$> decimal

pSlurmTime :: Parser (Quantity SlurmDateTime)
pSlurmTime = (Unset <$ ("N/A" <|> "(null)")) <|> (Set <$> pSlurmDateTime)

-- | Map 3-letter month abbreviations to integers 1-12 via ByteString matching.
monthToByte :: ByteString -> Int
monthToByte m = case m of
    "Jan" -> 1
    "Feb" -> 2
    "Mar" -> 3
    "Apr" -> 4
    "May" -> 5
    "Jun" -> 6
    "Jul" -> 7
    "Aug" -> 8
    "Sep" -> 9
    "Oct" -> 10
    "Nov" -> 11
    "Dec" -> 12
    _ -> 0

-- | Parses Slurm durations: [D-]HH:MM:SS
pDiffTime :: Parser (Quantity DiffTime)
pDiffTime = do
    bs <- A.takeTill (\c -> c == '|' || c == '\n')
    if bs == "N/A" || bs == "(null)" || B8.null bs
        then pure Unset
        else case parseSlurmDuration (B8.unpack bs) of
            Just d -> pure (Set d)
            Nothing -> pure Unset

parseSlurmDuration :: String -> Maybe DiffTime
parseSlurmDuration s = do
    let (days, rest) = case break (== '-') s of
            (d, '-' : r) -> (read d, r)
            _ -> (0, s)
        parts = B8.split ':' (B8.pack rest)
     in case reverse parts of
            [s', m, h] ->
                Just $
                    secondsToDiffTime $
                        (fromIntegral days * 86400) + (readInt h * 3600) + (readInt m * 60) + readInt s'
            [m, s'] -> Just $ secondsToDiffTime $ (readInt m * 60) + readInt s'
            _ -> Nothing
  where
    readInt = read . B8.unpack
