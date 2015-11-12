module Widget.RunResult (
    runResultWidget
) where

import Import
import Model.Activity

--runResultWidget :: Maybe (Entity RunResult) -> Widget
runResultWidget :: Activity -> Widget
runResultWidget activity = $(widgetFile "run-result")
