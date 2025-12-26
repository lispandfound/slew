module Logic.EventHandlers (
    Command (..),
    scontrolCommand,
    shouldTriggerRefresh,
) where

import Model.Job (Job (..))
import Optics.Operators ((^.))

-- | Commands that can be sent to slurm
data Command = Cancel | Suspend | Resume | Hold | Release | Top deriving (Show, Eq)

-- | Determine the scontrol command and arguments for a given command and job
-- Returns (command_name, arguments)
scontrolCommand :: Command -> Job -> (Text, [Text])
scontrolCommand Cancel job = ("scancel", [show (job ^. #jobId)])
scontrolCommand Suspend job = ("scontrol", ["suspend", show (job ^. #jobId)])
scontrolCommand Resume job = ("scontrol", ["resume", show (job ^. #jobId)])
scontrolCommand Hold job = ("scontrol", ["hold", show (job ^. #jobId)])
scontrolCommand Release job = ("scontrol", ["release", show (job ^. #jobId)])
scontrolCommand Top job = ("scontrol", ["top", show (job ^. #jobId)])

-- | Determine if a command should trigger a queue refresh
shouldTriggerRefresh :: Command -> Bool
shouldTriggerRefresh Cancel = True
shouldTriggerRefresh Suspend = True
shouldTriggerRefresh Resume = True
shouldTriggerRefresh Hold = True
shouldTriggerRefresh Release = True
shouldTriggerRefresh Top = True
