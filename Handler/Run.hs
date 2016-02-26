module Handler.Run where

import Import
import Model.Activity
import Network.Wai (lazyRequestBody)
import Data.Aeson (decode, Object)
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock.POSIX
import System.Timeout
import Text.Read
import Database.Persist
import Database.Persist.Sql (fromSqlKey, toSqlKey)
--import Database.Persist.Types
--import Database.Persist.Postgresql
--import Database.Persist.TH


import System.Process
import Settings.Environment

postRunR :: Handler Value
postRunR = do (Entity userId _) <- requireAuth
              let uId = userId
              (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
              let mUserName = profileUsername profile
              req <- reqWaiRequest <$> getRequest
              body <- liftIO $ lazyRequestBody req
              let bdson = (decode body)::(Maybe RunRequest)
              case (bdson,mUserName) of               
              	         (Just (RunRequest ac jas imgsz maxruntm snipid ispublic), username) -> let activity = activityFromId ac
                                                                                                    thecode = (activityHiddenCodeAbove activity) ++ jas ++ (activityHiddenCodeBelow activity) 
                                                                                                    msnipid = if snipid == "none" 
                                                                                                                then Nothing
                                                                                                                else Just (toSqlKey $ (read $ unpack snipid))
                                                                                                    codeOutput = writeAndRunGHC activity uId username jas thecode ((read $ unpack imgsz):: Int) ((read $ unpack maxruntm) :: Int) msnipid ((read $ unpack ispublic)::Bool)
                                                                                                in (liftM makeMessage $ codeOutput)
              	         _                                                                    -> return runError 


runError = object [ "stdout" .= (""::Text), "stderr" .= (""::Text), "error" .= ("run error"::Text), "imageid" .= (""::Text) ]         
makeMessage (stdo,stde,excode,fnm) = object [ "stdout" .= stdo, "stderr" .= stde, "error" .= excode, "imageid" .= fnm ]         



getUserName uid = do (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile uid
                     return $ profileUsername profile
                         
-- data structure to help parse the json from the request for postrunr              
data RunRequest = RunRequest {activity :: Text , text :: Text , imageSize :: Text, maxRunningTime :: Text, creatorSnippetId :: Text, isPublic :: Text}
instance FromJSON RunRequest where
 	parseJSON (Object v) = RunRequest <$> 
                          v .: "activity" <*>
                          v .: "file" <*>
                          v .: "imageSize" <*>
                          v .: "maxRunningTime" <*>
                          v .: "creatorSnippetId" <*>
                          v .: "isPublic"
 	parseJSON _ = mzero



writeAndRunGHC :: Activity -> UserId -> Text -> Text -> Text -> Int -> Int -> Maybe StoredSnippetId -> Bool -> HandlerT App IO (Text, Text, Text, Text)
writeAndRunGHC activity uId usernm enteredCode 
                          thecode imgsz maxruntm snipid isimagepublic =  do  tim <- liftIO $ liftM show $ round `fmap` getPOSIXTime
                                                                             localBuildingPath' <- liftIO $ localBuildingPath
                                                                             let fileName = usernm ++ ("_"::Text) ++ (pack tim)
                                                                             let fnm = localBuildingPath' ++ "hsfiles/" ++ fileName ++ ".hs"
                                                                             writeFile (unpack fnm) thecode
                                                                             let cmd = ("bash " ++ localBuildingPath' ++ "makecontainerandrun.sh " ++ fileName ++ " " ++ localBuildingPath' ++ " " ++ (pack $ show $ hasImageResult activity)) ++ " " ++ (pack $ show $ imgsz) 
                                                                             globalTimeLimitOnRuns' <- liftIO $ globalTimeLimitOnRuns
                                                                             let timeLimit = min globalTimeLimitOnRuns' (1000000*maxruntm)
                                                                             outfromrun <- liftIO $ timeout timeLimit $ readCreateProcessWithExitCode (shell (unpack cmd)) ""
                                                                             case outfromrun of
                                                                               Nothing -> do let cmdtostop = ("sh " ++ localBuildingPath' ++ "stopcontainer.sh " ++ fileName ++ " " ++ localBuildingPath')
                                                                                             _ <- liftIO $ readCreateProcessWithExitCode (shell (unpack cmdtostop)) ""
                                                                                             return ((""::Text) ,(""::Text),(("Run timeout! The program is only allowed " ++ (pack $ show timeLimit) ++ " microseconds")::Text),(""::Text))
                                                                               Just (ecd, stdout, stderr) -> do imid <- if hasImageResult activity 
                                                                                                                          then writeImageInDatabase uId "" (fileName ++ ".jpeg") activity isimagepublic enteredCode thecode snipid
                                                                                                                          else return Nothing
                                                                                                                let imidout = case imid of
                                                                                                                       Just imd  -> pack $ show (fromSqlKey imd)
                                                                                                                       Nothing   -> "noimage"
                                                                                                                return (pack stdout, pack stderr, pack $ show ecd, imidout)



--writeImageInDatabase owner title fileName activity public enteredCode fullCode created modified creatorSnippet
--savemSnippetInDb :: (Maybe StoredSnippetId) -> Text -> Text -> UserId -> Bool -> Text -> HandlerT App IO (Maybe StoredSnippetId)
--savemSnippetInDb mSid ac tit userId isPublic theCode = do 
--  now <- liftIO getCurrentTime
--  case mSid of 
--    Nothing  -> runDB $ do mid <- insertUnique $ StoredSnippet ac tit isPublic userId (contentHash theCode) now now theCode
--                           return mid
--    Just sid -> runDB $ do update sid [StoredSnippetSnippetContent =. theCode, StoredSnippetSnippetModified =. now, StoredSnippetSnippetTitle =. tit]
--                           return $ Just sid

writeImageInDatabase :: UserId -> Text -> Text -> Activity -> Bool -> Text -> Text -> (Maybe StoredSnippetId) -> HandlerT App IO (Maybe ImageId)
writeImageInDatabase owner title fileName activity public enteredCode fullCode creatorSnippet = do
  now <- liftIO getCurrentTime
  runDB $ do mid <- insertUnique $ Image owner title fileName (activityTitle activity) public enteredCode fullCode now now creatorSnippet
             return mid


    


