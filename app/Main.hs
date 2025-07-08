{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import qualified Brick.BChan as BC
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TChan (newTChanIO)
import qualified Graphics.Vty as V
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.AppState
import Options.Applicative
import SQueue.Poller
import UI.Event (handleEvent)
import UI.Poller (startPoller)
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
        , (attrName "jobLabel", fg V.cyan)
        , (attrName "jobValue", fg V.white)
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
    eventChannel <- BC.newBChan 10
    commandChannel <- newTChanIO
    opts <- execParser cli
    withAsync (squeueThread (pollInterval opts) SQueueStatus eventChannel) $ \_ ->
        withAsync (startPoller commandChannel (BC.writeBChan eventChannel . PollEvent)) $ \_ -> do
            let buildVty = mkVty defaultConfig
                is = initialState commandChannel
            initialVty <- buildVty
            void $ customMain initialVty buildVty (Just eventChannel) app is
