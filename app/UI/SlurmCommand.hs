{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.SlurmCommand (
    scontrolLog,
    drawSlurmCommandLog,
    SlurmCommandLogState (..),
    logSlurmCommandEvent,
) where

import Brick (
    EventM,
    Padding (Pad),
    ViewportType (Vertical),
    Widget,
    padLeft,
    txt,
    vBox,
    viewport,
    (<+>),
 )
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Model.SlurmCommand (
    SlurmCommandResult (..),
    SlurmContext (..),
    SlurmError (..),
 )
import Optics.Core ((^.))
import Optics.State.Operators ((%=))

data SlurmCommandLogState n = SlurmCommandLogState
    { log :: [SlurmCommandResult ()]
    , name :: n
    }
    deriving (Generic)

-------------------------------------------------------------------------------
-- UI Drawing
-------------------------------------------------------------------------------

drawEntry :: SlurmCommandResult () -> Widget n
drawEntry res =
    vBox [commandLine, padLeft (Pad 2) resultArea]
  where
    ctx = res ^. #context

    commandLine = txt $ "$ " <> (toText $ ctx ^. #cmd) <> " " <> (unwords . map toText $ ctx ^. #args)

    resultArea = case res ^. #result of
        -- On Execution Error, show the stderr
        Left (ExecutionError ec) ->
            txt ("[Exit " <> show ec <> "] ") <+> txt (ctx ^. #stderr)
        -- On Parsing Error, show the error message and the full stdout
        Left (DecodingError err) ->
            txt "[Parse Error] " <+> txt err <+> txt "\n" <+> txt (ctx ^. #stdout)
        -- On Success, show the first 20 lines of stdout
        Right _ ->
            let top20 = unlines . take 20 . lines $ (ctx ^. #stdout)
             in txt top20

drawSlurmCommandLog :: (Ord n, Show n) => SlurmCommandLogState n -> Widget n
drawSlurmCommandLog st =
    centerLayer $
        borderWithLabel (txt "Slurm Command Log") $
            viewport (st ^. #name) Vertical . vBox $
                map drawEntry (reverse $ st ^. #log)

-------------------------------------------------------------------------------
-- State Management
-------------------------------------------------------------------------------

scontrolLog :: n -> SlurmCommandLogState n
scontrolLog name' =
    SlurmCommandLogState
        { log = mempty
        , name = name'
        }

logSlurmCommandEvent :: SlurmCommandResult () -> EventM n (SlurmCommandLogState n) ()
logSlurmCommandEvent entry = #log %= (entry :)
