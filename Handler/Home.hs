module Handler.Home where

import Import
import Util.Util
import Model.Snippet
import Model.Activity
import Widget.Activities
import Settings.Environment
import Widget.LessonList
import Yesod.Auth.Simple
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)
    
import System.Directory

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    mUserId <- maybeAuthId
    case mUserId of     
      Nothing -> defaultLayout $ do 
                        aDomId <- newIdent
                        setTitle "Mathvas"
                        $(widgetFile "welcome")
                        
      Just _ -> defaultLayout $ do
                       aDomId <- newIdent
                       setTitle "Mathvas!"
                       $(widgetFile "homepage")
                       lessonsPath' <- liftIO lessonsPath
                       (lift (parseLessonList $ ((unpack lessonsPath' ++ "lesson_list")::FilePath))) >>= (lessonListWidget (Just "Tutorials")) 
                       activitiesWidget $ Just "Activities"


postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField (withSmallInput "What's on the file?") Nothing
