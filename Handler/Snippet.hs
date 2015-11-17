module Handler.Snippet where

import Import
import Model.Activity
import Model.Snippet
import Widget.Editor
import Widget.RunResult
import Text.Julius (rawJS)
import Database.Persist.Sql (fromSqlKey)

getSnippetR :: StoredSnippetId -> Handler Html
getSnippetR sId = do	    
            userId <- requireAuthId
            let isExistingSnippet = True
            let snippetId = show (fromSqlKey $ sId)
            mSnippet <- runDB $ get sId
            case mSnippet of 
                Nothing         -> error "There was a problem retrieving this snippet"
                Just strsnippet -> let snippet = fromStoredSnippet strsnippet
                                       activity = snippetActivity snippet
                                       activityName = activityId activity
                                   in if (storedSnippetSnippetOwner strsnippet == userId) then
                                              defaultLayout $ do
                                              aDomId <- newIdent
                                              addScript $ StaticR lib_ace_ace_js 
                                              setTitle "Studio Math!"
                                              $(widgetFile "homepage")
                                              $(widgetFile "compose")
                                      else error "There was a problem with your access permissions to this code." 


deleteSnippetR :: StoredSnippetId -> Handler Value
deleteSnippetR sId = do
    mUserId <- requireAuthId
    runDB $ do delete sId
    return $ object []
