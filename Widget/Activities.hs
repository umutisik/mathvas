module Widget.Activities (
    activitiesWidget
) where

import Import
import Model.Activity

activitiesWidget :: Maybe Text -> Widget
activitiesWidget mTitle =
    $(widgetFile "activities")
