{-# LANGUAGE NumericUnderscores #-}

module Main where

import Brick (
    App (..),
    customMain,
    showFirstCursor,
    zoom,
 )
import Brick.BChan qualified as BC
import Brick.Themes (Theme, themeToAttrMap)
import Brick.Types (EventM)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.CrossPlatform (mkVty)
import Model.AppState (
    AppState (..),
    Name,
    SlewEvent (SQueueStatus, Tick),
    initialState,
 )
import Model.Options (Options (Options, pollInterval, theme))
import Model.SlurmCommand (squeue)
import Optics.Operators ((^.))
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, short, showDefault, str, value)
import Slurm.Channel (runJsonErr, worker)
import System.Environment.Blank (getEnv)
import System.FilePath (combine)
import UI.Echo (echo)
import UI.Event (handleEvent)
import UI.Themes (defaultTheme, loadTheme)
import UI.View (drawApp)

------------------------------------------------------------
-- Brick App Definition

app :: EventM Name AppState () -> Theme -> App AppState SlewEvent Name
app init' theme =
    App
        { appDraw = drawApp
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
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

tickThread :: Int -> IO () -> IO ()
tickThread delay callback = forever (callback >> threadDelay delay)

configDirectory :: IO (Maybe FilePath)
configDirectory = do
    home <- getEnv "HOME"
    xdgConfig <- getEnv "XDG_CONFIG_HOME"
    let
        defaultConfigDir = combine <$> home <*> pure ".config"
        config = xdgConfig <|> defaultConfigDir
    return $ combine <$> config <*> pure "slew"

withAsyncs :: [IO a] -> IO b -> IO b
withAsyncs asyncs action = foldr (\async form -> withAsync async (const form)) action asyncs

seconds :: Int
seconds = 1_000_000

main :: IO ()
main = do
    opts <- execParser cli
    eventChannel <- BC.newBChan 50
    workerChannel <- BC.newBChan 50

    is <- initialState workerChannel opts
    slewConfigDirectory <- configDirectory
    let slewThemePath = (opts ^. #theme) <|> combine <$> slewConfigDirectory <*> pure "theme.ini"
        asyncActions =
            [ tickThread (1 * seconds) (BC.writeBChan eventChannel Tick)
            , tickThread (opts ^. #pollInterval * seconds) (runJsonErr workerChannel squeue SQueueStatus)
            , worker workerChannel eventChannel
            ]
    themeOrErr <- maybe (pure . Right $ defaultTheme) loadTheme slewThemePath
    withAsyncs asyncActions $ do
        let buildVty = mkVty defaultConfig
            theme = fromRight defaultTheme themeOrErr
            initialEvent = either (zoom #echoState . echo . toText) (const $ pure ()) themeOrErr
        initialVty <- buildVty
        void $ customMain initialVty buildVty (Just eventChannel) (app initialEvent theme) is
