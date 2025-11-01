{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick (
    App (..),
    customMain,
    showFirstCursor,
    zoom,
 )
import qualified Brick.BChan as BC
import Brick.Themes (Theme, themeToAttrMap)
import Brick.Types (EventM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.AppState (
    AppState (..),
    Name,
    SlewEvent (SQueueStatus, SlurmCommandReceive, Tick),
    initialState,
 )
import Model.Options
import Optics.Operators ((^.))
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, short, showDefault, str, value)
import SQueue.Poller (squeueThread)
import System.Environment.Blank (getEnv)
import System.FilePath (combine)
import UI.Echo (echo)
import UI.Event (handleEventWithEcho)
import UI.SlurmCommand (SlurmCommandLogEntry (..), SlurmCommandLogState (..), startSlurmCommandListener)
import UI.Themes (defaultTheme, loadTheme)
import UI.View (drawApp)

------------------------------------------------------------
-- Brick App Definition

app :: EventM Name AppState () -> Theme -> App AppState SlewEvent Name
app init' theme =
    App
        { appDraw = drawApp
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEventWithEcho
        , appStartEvent = init'
        , appAttrMap = const (themeToAttrMap theme)
        }

------------------------------------------------------------
-- Main

options :: Parser Options
options = Options <$> pollInterval <*> theme <*> tailCommand
  where
    pollInterval = option auto (long "interval" <> short 'i' <> help "Polling interval for squeue commands. Keep short to keep your admins happy. Does not affect output viewing." <> showDefault <> value 30 <> metavar "TIME (s)")
    theme = optional (option str (long "theme" <> short 't' <> help "Path to a custom theme file" <> showDefault <> metavar "FILE"))
    tailCommand = option auto (long "tail" <> help "Tail command for C-o and C-e commands. Use %f as a placeholder for stdout/stderr files." <> value "less +F %f" <> showDefault <> metavar "COMMAND")

cli :: ParserInfo Options
cli = info (options <**> helper) (fullDesc <> header "slew - Slurm for the rest of us.")

tickThread :: IO () -> IO ()
tickThread callback = forever (callback >> threadDelay 1_000_000)

configDirectory :: IO (Maybe FilePath)
configDirectory = do
    home <- getEnv "HOME"
    xdgConfig <- getEnv "XDG_CONFIG_HOME"
    let
        defaultConfigDir = combine <$> home <*> pure ".config"
        config = xdgConfig <|> defaultConfigDir
    return $ combine <$> config <*> pure "slew"

withAsyncs :: [IO a] -> IO b -> IO b
withAsyncs asyncs action = foldr (\async form -> withAsync async (\_ -> form)) action asyncs

main :: IO ()
main = do
    opts <- execParser cli
    is <- initialState opts
    eventChannel <- BC.newBChan 50
    slewConfigDirectory <- configDirectory
    let slewThemePath = (opts ^. #theme) <|> combine <$> slewConfigDirectory <*> pure "theme.ini"
        asyncActions =
            [ tickThread (BC.writeBChan eventChannel Tick)
            , squeueThread (opts ^. #pollInterval) (is ^. #squeueChannel) (BC.writeBChan eventChannel . SQueueStatus)
            , startSlurmCommandListener (is ^. #scontrolLogState ^. #chan) (\cmd output -> BC.writeBChan eventChannel (SlurmCommandReceive (SlurmCommandLogEntry cmd output)))
            ]
    themeOrErr <- maybe (pure . Right $ defaultTheme) loadTheme slewThemePath
    withAsyncs asyncActions $ do
        let buildVty = mkVty defaultConfig
            theme = fromRight defaultTheme themeOrErr
            initialEvent = either (zoom #echoState . echo . toText) (const $ pure ()) themeOrErr
        initialVty <- buildVty
        void $ customMain initialVty buildVty (Just eventChannel) (app initialEvent theme) is
