module Handler.Snippets where

import Import
import Model.Snippet
import Model.Activity
import Database.Persist
import Data.Time.ISO8601 (parseISO8601)
import Data.Maybe (fromJust)

getSnippetsR :: Handler Html
getSnippetsR = do	    
            userId <- requireAuthId
            (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
            let userName = profileUsername profile
            snippetList <- runDB $ selectList [StoredSnippetSnippetOwner ==. userId] [Desc StoredSnippetSnippetModified]
            defaultLayout $ do
                                              aDomId <- newIdent
                                              setTitle "Studio Math!"
                                              $(widgetFile "homepage")
                                              $(widgetFile "snippets")


utcFormat :: UTCTime -> Text
utcFormat time = pack $ formatTime defaultTimeLocale "%c" time

iso8601Format :: Text -> Text
iso8601Format time = utcFormat $ fromJust $ parseISO8601 $ unpack time

