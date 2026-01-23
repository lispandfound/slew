module UI.Menus (
    scontrolTransient,
    sortTransient,
    sortListByCat,
) where

import Brick.Widgets.Core (txt, withAttr)
import Model.AppState (Category (..), Filter (..), Name, SlewEvent (..))
import Model.Job (Job (..))
import Model.SlurmCommand (cancel, hold, resume, suspend, top)
import Optics.Getter (view)
import Optics.Label ()
import UI.Themes (header, transient)
import UI.Transient qualified as TR

scontrolTransient :: TR.TransientState SlewEvent Name
scontrolTransient =
    TR.menu "Job Control" $
        TR.horizontalLayout
            [ TR.submenu 's' "State Control" $
                TR.horizontalLayout
                    [ TR.verticalLayoutWithLabel
                        (withAttr (transient <> header) $ txt "Stop or Start")
                        [ TR.item 'h' "Hold" (SlurmCommandSend hold)
                        , TR.item 'r' "Release" (SlurmCommandSend hold)
                        , TR.item 'R' "Resume" (SlurmCommandSend resume)
                        , TR.item 's' "Suspend" (SlurmCommandSend suspend)
                        , TR.item 'c' "Cancel" (SlurmCommandSend cancel)
                        ]
                    , TR.verticalLayoutWithLabel
                        (withAttr (transient <> header) $ txt "Priority")
                        [ TR.item 't' "Top" (SlurmCommandSend top)
                        ]
                    ]
            ]

sortTransient :: TR.TransientState SlewEvent Name
sortTransient =
    TR.menu "Job Sorting and Filtering" $
        TR.horizontalLayout
            [ TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Name of")
                [ TR.item
                    'a'
                    "Account"
                    (SortBy Account)
                , TR.item
                    'u'
                    "User"
                    (SortBy UserName)
                , TR.item 'j' "Job" (SortBy JobName)
                ]
            , TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Time")
                [ TR.item 's' "Start Time" (SortBy StartTime)
                , TR.item 'e' "End Time" (SortBy EndTime)
                ]
            , TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Resources")
                [ TR.item 'c' "CPUs" (SortBy CPUs)
                , TR.item 'm' "Memory (per node)" (SortBy Memory)
                ]
            , TR.verticalLayoutWithLabel
                (withAttr (transient <> header) $ txt "Filtering (triggers squeue)")
                [ TR.item 'm' "Me" (FilterBy User)
                , TR.item 'a' "All" (FilterBy NoFilter)
                ]
            ]

sortListByCat :: Category -> Job -> Job -> Ordering
sortListByCat Account = comparing (view #account)
sortListByCat CPUs = comparing (view #cpus)
sortListByCat StartTime = comparing (view #startTime)
sortListByCat EndTime = comparing (view #endTime)
sortListByCat JobName = comparing (view #name)
sortListByCat UserName = comparing (view #userName)
sortListByCat Memory = comparing (view #memoryPerNode)
