module UI.Themes where

import Brick
import Brick.Themes (Theme, loadCustomizations, newTheme)
import Control.Exception (catch)
import GHC.IO.Exception (IOException)
import Graphics.Vty (Attr, blue, bold, cyan, defAttr, green, red, reverseVideo, underline, yellow)
import Graphics.Vty.Attributes (withStyle)

selectedRow :: AttrName
selectedRow = attrName "selected"

highlight :: AttrName
highlight = attrName "highlight"

job :: AttrName
job = attrName "job"

jobState :: AttrName
jobState = job <> attrName "state"

pending :: AttrName
pending = jobState <> attrName "PENDING"

running :: AttrName
running = jobState <> attrName "RUNNING"

completed :: AttrName
completed = jobState <> attrName "COMPLETED"

failed :: AttrName
failed = jobState <> attrName "FAILED"

cancelled :: AttrName
cancelled = jobState <> attrName "CANCELLED"

exit :: AttrName
exit = attrName "exit"

failure :: AttrName
failure = exit <> attrName "failure"

success :: AttrName
success = exit <> attrName "success"

label :: AttrName
label = attrName "label"

jobLabel :: AttrName
jobLabel = job <> label

jobId :: AttrName
jobId = job <> attrName "id"

squeue :: AttrName
squeue = attrName "squeue"

column :: AttrName
column = attrName "column"

header :: AttrName
header = attrName "header"

submenu :: AttrName
submenu = attrName "submenu"

transient :: AttrName
transient = attrName "transient"

defaultAttrMap :: [(AttrName, Attr)]
defaultAttrMap =
    [ (selectedRow, withStyle defAttr reverseVideo)
    , (highlight, fg cyan)
    , (pending, fg yellow)
    , (running, fg green)
    , (completed, fg blue)
    , (failed, fg red)
    , (cancelled, fg red)
    , (failure, fg red)
    , (success, fg green)
    , (jobLabel, fg cyan)
    , (jobId, fg blue)
    , (transient <> header, withStyle defAttr underline)
    , (transient <> label, withStyle (fg blue) bold)
    , (transient <> submenu, withStyle (fg blue) underline)
    ]

defaultTheme :: Theme
defaultTheme = newTheme defAttr defaultAttrMap

loadTheme :: FilePath -> IO (Either String Theme)
loadTheme themeFile = loadCustomizations themeFile defaultTheme `catch` (\e -> pure . Left . show $ (e :: IOException))
