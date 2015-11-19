module Handler.Lesson where

import Import
import Yesod.Markdown
import Model.Activity
import Model.Snippet
import Widget.Editor
import Widget.RunResult
import Text.Julius (rawJS)
import Database.Persist.Sql (toSqlKey)



getLessonR :: Text -> Handler Html
getLessonR text = do
    userId <- requireAuthId
    let handlerName = "getLessonR" :: Text
    defaultLayout $ do
        addScript $ StaticR lib_ace_ace_js 
        setTitle "Studio Math!"
        $(widgetFile "homepage")
        let mm = Markdown $ ("# Hellodeneme\n This is some markdown \n\n Yes. And here is the editorr"::Text)
        [whamlet| #{mm} |]
        let numberOfLines = 14
            sId = toSqlKey 0
            snippetId = ("" :: Text)
            activityName = "images"::Text
            isExistingSnippet = False
            activity = activityFromId activityName
            snippet = (defaultSnippet activity)
        $(widgetFile "compose") 
        $(widgetFile "compose") 
        [whamlet| ##{mm} |]
        editorWidget (defaultSnippet $ activityFromId "openHaskell") 10
        editorWidget (defaultSnippet $ activityFromId "openHaskell") 10
                -- for all editors to work, they need to have different id numbers
                -- what i need here is a custom small-compose widget
                -- probably for now, best to keep to single editor though and just focus on the markup
                -- another caveat: if you need extra hidden haskell code, then how is RUN going to learn about it?
