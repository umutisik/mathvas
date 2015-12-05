module Widget.LessonList (
    lessonListWidget
) where

import Import

lessonListWidget :: Maybe Text -> [(Text,Text)] -> WidgetT App IO ()
lessonListWidget mTitle lsnLst =  
    $(widgetFile "widget/lessonlist")

