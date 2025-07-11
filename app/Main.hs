{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick (
    App (..),
    AttrMap,
    attrMap,
    attrName,
    customMain,
    fg,
    showFirstCursor,
 )
import qualified Brick.BChan as BC
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.AppState (
    AppState (..),
    Name,
    SlewEvent (PollEvent, SQueueStatus, SlurmCommandReceive, Tick),
    initialState,
 )
import Optics.Operators ((^.))
import Options.Applicative (
    Parser,
    ParserInfo,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    short,
    showDefault,
    value,
 )
import SQueue.Poller (squeueThread)
import UI.Event (handleEvent)
import UI.Poller (PollerState (..), startPoller)
import UI.SlurmCommand (SlurmCommandLogEntry (..), SlurmCommandLogState (..), startSlurmCommandListener)
import UI.View (drawApp)

------------------------------------------------------------
-- Brick App Definition

app :: App AppState SlewEvent Name
app =
    App
        { appDraw = drawApp
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = pure ()
        , appAttrMap = const appAttrs
        }

------------------------------------------------------------
-- Attribute Styles

appAttrs :: AttrMap
appAttrs =
    attrMap
        V.defAttr
        [ (attrName "selected", V.withStyle V.defAttr V.reverseVideo)
        , (attrName "highlight", fg V.cyan)
        , (attrName "jobState.PENDING", fg V.yellow)
        , (attrName "jobState.RUNNING", fg V.green)
        , (attrName "jobState.COMPLETED", fg V.blue)
        , (attrName "jobState.FAILED", fg V.red)
        , (attrName "jobState.CANCELLED", fg V.red)
        , (attrName "exitFailure", fg V.red)
        , (attrName "exitSuccess", fg V.green)
        , (attrName "jobLabel", fg V.cyan)
        , (attrName "jobValue", fg V.white)
        , (attrName "jobId", fg V.blue)
        ]

------------------------------------------------------------
-- Main

data Options = Options
    {pollInterval :: Int}
    deriving (Generic)

options :: Parser Options
options = Options <$> (option auto (long "interval" <> short 'i' <> help "Polling interval for squeue commands. Keep short to keep your admins happy. Does not affect output viewing." <> showDefault <> value 30 <> metavar "TIME (s)"))

cli :: ParserInfo Options
cli = info (options <**> helper) (fullDesc <> header "slew - Slurm for the rest of us.")

tickThread :: IO () -> IO ()
tickThread callback = forever (callback >> threadDelay 1_000_000)

main :: IO ()
main = do
    is <- initialState
    eventChannel <- BC.newBChan 50
    opts <- execParser cli
    withAsync (tickThread (BC.writeBChan eventChannel Tick)) $ \_ ->
        withAsync (squeueThread (opts ^. #pollInterval) (is ^. #squeueChannel) (BC.writeBChan eventChannel . SQueueStatus)) $ \_ ->
            withAsync (startPoller (is ^. #pollState ^. #commandChannel) (BC.writeBChan eventChannel . PollEvent)) $ \_ -> do
                withAsync (startSlurmCommandListener (is ^. #scontrolLogState ^. #chan) (\cmd output -> BC.writeBChan eventChannel (SlurmCommandReceive (SlurmCommandLogEntry cmd output)))) $ \_ -> do
                    let buildVty = mkVty defaultConfig

                    initialVty <- buildVty
                    void $ customMain initialVty buildVty (Just eventChannel) app is
