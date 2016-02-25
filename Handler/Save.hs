module Handler.Save where

import Import
import Model.Snippet
import Model.Activity
import Network.Wai (lazyRequestBody)
import Data.Aeson (decode, Object)
import Data.Aeson.Types (parseMaybe)



postSaveR :: StoredSnippetId -> Handler Value
postSaveR idd = maybeSaveR $ Just idd


maybeSaveR :: (Maybe StoredSnippetId) -> Handler Value
maybeSaveR mSid = do (Entity userId _) <- requireAuth
                     (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
                     let userName = profileUsername profile
                     req <- reqWaiRequest <$> getRequest
                     body <- liftIO $ lazyRequestBody req
                     let bdson = (decode body)::(Maybe SaveRequest)
                     case bdson of               
              	         (Just (SaveRequest ac tit publ cod)) -> let --activity = activityFromId ac
                                                                     snippetId = savemSnippetInDb mSid ac tit userId publ cod
              	                                                 in (liftM makeMessage $ snippetId)
              	         _                                    -> return saveError 

makeMessage sid = object [ "snippetId" .= sid]         
saveError = object [ "error" .= ("Save Error"::Text) ]         


data SaveRequest = SaveRequest {activity :: Text , title :: Text, public :: Bool, code :: Text }
instance FromJSON SaveRequest where
 	parseJSON (Object v) = SaveRequest <$> 
                          v .: "activity" <*>
                          v .: "title" <*>
                          v .: "public" <*>
                          v .: "file"
 	parseJSON _ = mzero

savemSnippetInDb :: (Maybe StoredSnippetId) -> Text -> Text -> UserId -> Bool -> Text -> HandlerT App IO (Maybe StoredSnippetId)
savemSnippetInDb mSid ac tit userId isPublic theCode = do 
	now <- liftIO getCurrentTime
	case mSid of 
	  Nothing  -> runDB $ do mid <- insertUnique $ StoredSnippet ac tit isPublic userId (contentHash theCode) now now theCode
	                         return mid
	  Just sid -> runDB $ do update sid [StoredSnippetSnippetContent =. theCode, StoredSnippetSnippetModified =. now, StoredSnippetSnippetTitle =. tit]
	                         return $ Just sid


    
              
--snippetId Text
--    snippetActivity Text
--    snippetTitle Text
--    snippetPublic Bool
--    snippetOwner Text
--    snippetHash Text
--    snippetModified UTCTime
--    snippetCreated UTCTime
--    snippetContent Text
    


--deleteSnippetR :: (MaybeText) -> Handler Html
--deleteSnippetR text = error "Not yet implemented: deleteSnippetR"
