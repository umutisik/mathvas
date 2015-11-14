module Handler.Run where

import Import
import Model.Activity
import Network.Wai (lazyRequestBody)
import Data.Aeson (decode, Object)
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock.POSIX
import System.Timeout

import System.Process

postRunR :: Handler Value
postRunR = do mUserId <- requireAuthId
              req <- reqWaiRequest <$> getRequest
              body <- liftIO $ lazyRequestBody req
              let bdson = (decode body)::(Maybe RunRequest)
              case bdson of               
              	         Just (RunRequest ac jas) -> let activity = activityFromId ac
                                                         thecode =(activityHiddenCodeAbove activity) ++ jas ++ (activityHiddenCodeBelow activity) 
              	                                         codeOutput = writeAndRunGHC tempDefaultUserId thecode
              	                                     in liftIO (liftM makeMessage $ codeOutput)
              	         _ -> return runError

runError = object [ "stdout" .= (""::Text), "stderr" .= (""::Text), "error" .= ("run error"::Text), "localfilename" .= (""::Text) ]         
makeMessage (stdo,stde,excode,fnm) = object [ "stdout" .= stdo, "stderr" .= stde, "error" .= excode, "localfilename" .= fnm ]         

-- data structure to help parse the json from the request for postrunr              
data RunRequest = RunRequest {activity :: Text , text :: Text }
instance FromJSON RunRequest where
 	parseJSON (Object v) = RunRequest <$> 
                          v .: "activity" <*>
                          v .: "file"
 	parseJSON _ = mzero


writeAndRunGHC :: Text -> Text -> IO (Text, Text, Text, Text)
writeAndRunGHC userid thecode =  do tim <- liftM show $ round `fmap` getPOSIXTime
                                    let fileName = userid ++ ("_"::Text) ++ (pack tim)
                                    let fnm = localBuildingPath ++ "hsfiles/" ++ fileName ++ ".hs"
                                    writeFile (unpack fnm) thecode
                                    let cmd = ("sh " ++ localBuildingPath ++ "makecontainerandrun.sh " ++ fileName ++ " " ++ localBuildingPath)
                                    outfromrun <- timeout timeLimitOnRuns $ readCreateProcessWithExitCode (shell (unpack cmd)) ""
                                    case outfromrun of
                                      Nothing -> do let cmdtostop = ("sh " ++ localBuildingPath ++ "stopcontainer.sh " ++ fileName ++ " " ++ localBuildingPath)
                                                    _ <- readCreateProcessWithExitCode (shell (unpack cmdtostop)) ""
                                                    return ((""::Text) ,(""::Text),(("Run timeout! The program is only allowed " ++ (pack $ show timeLimitOnRuns) ++ " microseconds")::Text),(""::Text))
                                      Just (ecd, stdout, stderr) -> return (pack stdout, pack stderr, pack $ show ecd, fileName)


-- these need to be fixed and replaced
-- by environment variables
localBuildingPath = "dockerSandboxes/"
tempDefaultUserId = "user1"::Text

--in microseconds
timeLimitOnRuns::Int
timeLimitOnRuns = 20000000 
