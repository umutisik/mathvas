module Handler.Compose where

import Import
import Model.Activity
import Model.Snippet
import Widget.Editor
import Widget.RunResult
import Text.Julius (rawJS)
import Database.Persist.Sql (toSqlKey)

getComposeR :: Text -> Handler Html
getComposeR activityName = do 
    userId <- requireAuthId
    let handlerName = "getComposeR" :: Text
    let activity = activityFromId activityName
    let isExistingSnippet = False
    let snippetId = ("" :: Text)
    let sId = toSqlKey 0
    let numberOfLines = 30
    defaultLayout $ do
        aDomId <- newIdent
        addScript $ StaticR lib_ace_ace_js 
        setTitle "Mathvas - Compose"
        let snippet = defaultSnippet $ activityFromId activityName
        $(widgetFile "homepage")
        $(widgetFile "compose")

