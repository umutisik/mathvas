module Handler.Run where

import Import
import Model.Activity
import Network.Wai (lazyRequestBody)
import Data.Aeson (decode, Object)
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock.POSIX
import System.Timeout
import Text.Read

import System.Process
import Settings.Environment

postRunR :: Handler Value
postRunR = do (Entity userId _) <- requireAuth
              --let mUserId = Just userId
              (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
              let mUserName = profileUsername profile
              req <- reqWaiRequest <$> getRequest
              body <- liftIO $ lazyRequestBody req
              let bdson = (decode body)::(Maybe RunRequest)
              case (bdson,mUserName) of               
              	         (Just (RunRequest ac jas imgsz maxruntm), username) -> let activity = activityFromId ac
                                                                                    thecode = (activityHiddenCodeAbove activity) ++ jas ++ (activityHiddenCodeBelow activity) 
              	                                                                    codeOutput = writeAndRunGHC activity username thecode ((read $ unpack imgsz):: Int) ((read $ unpack maxruntm) :: Int)
              	                                                                in liftIO (liftM makeMessage $ codeOutput)
              	         _                                    -> return runError 


runError = object [ "stdout" .= (""::Text), "stderr" .= (""::Text), "error" .= ("run error"::Text), "localfilename" .= (""::Text) ]         
makeMessage (stdo,stde,excode,fnm) = object [ "stdout" .= stdo, "stderr" .= stde, "error" .= excode, "localfilename" .= fnm ]         



getUserName uid = do (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile uid
                     return $ profileUsername profile
                         
-- data structure to help parse the json from the request for postrunr              
data RunRequest = RunRequest {activity :: Text , text :: Text , imageSize :: Text, maxRunningTime :: Text}
instance FromJSON RunRequest where
 	parseJSON (Object v) = RunRequest <$> 
                          v .: "activity" <*>
                          v .: "file" <*>
                          v .: "imageSize" <*>
                          v .: "maxRunningTime"
 	parseJSON _ = mzero



writeAndRunGHC :: Activity -> Text -> Text -> Int -> Int -> IO (Text, Text, Text, Text)
writeAndRunGHC activity userid thecode imgsz maxruntm =  do  tim <- liftM show $ round `fmap` getPOSIXTime
                                                             localBuildingPath' <- liftIO $ localBuildingPath
                                                             let fileName = userid ++ ("_"::Text) ++ (pack tim)
                                                             let fnm = localBuildingPath' ++ "hsfiles/" ++ fileName ++ ".hs"
                                                             writeFile (unpack fnm) thecode
                                                             let cmd = ("sh " ++ localBuildingPath' ++ "makecontainerandrun.sh " ++ fileName ++ " " ++ localBuildingPath' ++ " " ++ (pack $ show $ hasImageResult activity)) ++ " " ++ (pack $ show $ imgsz) 
                                                             globalTimeLimitOnRuns' <- liftIO $ globalTimeLimitOnRuns
                                                             let timeLimit = min globalTimeLimitOnRuns' (1000000*maxruntm)
                                                             outfromrun <- timeout timeLimit $ readCreateProcessWithExitCode (shell (unpack cmd)) ""
                                                             case outfromrun of
                                                               Nothing -> do let cmdtostop = ("sh " ++ localBuildingPath' ++ "stopcontainer.sh " ++ fileName ++ " " ++ localBuildingPath')
                                                                             _ <- readCreateProcessWithExitCode (shell (unpack cmdtostop)) ""
                                                                             return ((""::Text) ,(""::Text),(("Run timeout! The program is only allowed " ++ (pack $ show timeLimit) ++ " microseconds")::Text),(""::Text))
                                                               Just (ecd, stdout, stderr) -> return (pack stdout, pack stderr, pack $ show ecd, fileName)


