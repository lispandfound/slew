module UI.SlurmCommand (
    scontrolLog,
    drawSlurmCommandLog,
    SlurmCommandLogState (..),
    logSlurmCommandEvent,
) where

import Brick (EventM, Padding (Pad), ViewportType (Vertical), Widget, hBox, padLeft, txt, vBox, viewport, withAttr, (<+>))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (centerLayer)
import Data.Char (isDigit)
import Fmt (fmt, (+|), (|+))
import Model.SlurmCommand (
    SlurmCommandResult (..),
    SlurmContext (..),
    SlurmError (..),
 )
import Optics.Core ((^.))
import Optics.State.Operators ((%=))
import UI.Themes (jobId)

data SlurmCommandLogState n = SlurmCommandLogState
    { log :: [SlurmCommandResult ()]
    , name :: n
    }
    deriving (Generic)

takeLines :: Int -> Text -> Text
takeLines n = unlines . take n . lines

drawEntry :: SlurmCommandResult () -> Widget n
drawEntry res =
    vBox [commandLine, padLeft (Pad 2) resultArea]
  where
    ctx = res ^. #context
    commandLine = (txt . fmt) ("$ " +| ctx ^. #cmd |+ " ") <+> (hBox . map argFmt) (ctx ^. #args)
    resultArea = case res ^. #result of
        Left (ExecutionError ec) ->
            txt . fmt $ "[Exit" +| ec |+ "] " +| ctx ^. #stderr |+ ""
        Left (DecodingError err) ->
            txt . fmt $ "[Parse Error] " +| err |+ "\n" +| ctx ^. #stdout |+ ""
        Left TimeoutError ->
            txt "Timeout"
        Left (BrokenHandle _) ->
            txt "Broken handle"
        Right _ ->
            txt . takeLines 20 $
                (ctx ^. #stderr) <> "\n" <> (ctx ^. #stdout)

    argFmt arg = if all isDigit arg then (withAttr jobId . txt . toText) arg else (txt . toText) arg

drawSlurmCommandLog :: (Ord n, Show n) => SlurmCommandLogState n -> Widget n
drawSlurmCommandLog st =
    centerLayer $
        borderWithLabel (txt "Slurm Command Log") $
            viewport (st ^. #name) Vertical . vBox $
                map drawEntry (reverse $ st ^. #log)

scontrolLog :: n -> SlurmCommandLogState n
scontrolLog name' =
    SlurmCommandLogState
        { log = mempty
        , name = name'
        }

logSlurmCommandEvent :: SlurmCommandResult () -> EventM n (SlurmCommandLogState n) ()
logSlurmCommandEvent entry = #log %= (entry :)
