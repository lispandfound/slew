{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Brick.BChan as BC
import Control.Concurrent.Async (withAsync)
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.AppState
import Options.Applicative
import SQueue.Poller
import UI.Event (handleEvent)
import UI.Poller (commandChannel, startPoller)
import UI.SControl (SControlLogEntry (..), chan, startSControlListener)
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
        , (attrName "jobState.CANCELLED", V.withStyle V.defAttr V.reverseVideo)
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

options :: Parser Options
options = Options <$> (option auto (long "interval" <> short 'i' <> help "Polling interval for squeue commands. Keep short to keep your admins happy. Does not affect output viewing." <> showDefault <> value 30 <> metavar "TIME (s)"))

cli :: ParserInfo Options
cli = info (options <**> helper) (fullDesc <> header "slew - Slurm for the rest of us.")

main :: IO ()
main = do
    is <- initialState
    eventChannel <- BC.newBChan 10
    opts <- execParser cli
    withAsync (squeueThread (pollInterval opts) SQueueStatus eventChannel) $ \_ ->
        withAsync (startPoller (is ^. pollState ^. commandChannel) (BC.writeBChan eventChannel . PollEvent)) $ \_ -> do
            withAsync (startSControlListener (is ^. scontrolLogState ^. chan) (\cmd output -> BC.writeBChan eventChannel (SControlReceive (SControlLogEntry cmd output)))) $ \_ -> do
                let buildVty = mkVty defaultConfig

                initialVty <- buildVty
                void $ customMain initialVty buildVty (Just eventChannel) app is
