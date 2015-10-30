module Widget.RunResult (
    runResultWidget
) where

import Import

--runResultWidget :: Maybe (Entity RunResult) -> Widget
runResultWidget :: Widget
runResultWidget = $(widgetFile "run-result")
