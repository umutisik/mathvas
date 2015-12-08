module Handler.NewActivity where

import Import
import Widget.Activities
import Widget.LessonList
import Settings.Environment
import Util.Util

getNewActivityR :: Handler Html
getNewActivityR = do  mUserId <- requireAuthId
                      defaultLayout $ do setTitle "New snippet - Choose language"
                                         $(widgetFile "new")
                                         lessonsPath' <- liftIO $ lessonsPath
                                         (lift (parseLessonList $ ((unpack lessonsPath' ++ "lesson_list")::FilePath))) >>= (lessonListWidget (Just "Tutorials")) 

