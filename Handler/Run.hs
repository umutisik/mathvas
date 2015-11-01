module Handler.Run where

import Import
import Network.Wai (lazyRequestBody)
import Data.Aeson (decode, Object)
import Data.Aeson.Types (parseMaybe)
import Data.Time.Clock.POSIX

import System.Process

postRunR :: Handler Value
postRunR = do req <- reqWaiRequest <$> getRequest
              body <- liftIO $ lazyRequestBody req
              let bdson = (decode body)::(Maybe FileText)
              case bdson of               
              	         Just jas -> let thecode = text jas 
              	                         codeOutput = writeAndRunGHC tempDefaultUserId thecode
              	                     in liftIO (liftM makeMessage $ codeOutput)
              	         _ -> return runError

runError = object [ "stdout" .= (""::Text), "stderr" .= (""::Text), "error" .= ("run error"::Text) ]         
makeMessage (stdo,stde,excode,fnm) = object [ "stdout" .= stdo, "stderr" .= stde, "error" .= excode, "localfilename" .= fnm ]         

-- unnecessary data structure to help parse the json from the request for postrunr              
data FileText = FileText { text :: Text }
instance FromJSON FileText where
 	parseJSON (Object v) = FileText <$> v .: "file"
 	parseJSON _ = mzero


writeAndRunGHC :: Text -> Text -> IO (Text, Text, Text, Text)
writeAndRunGHC userid thecode =  do tim <- liftM show $ round `fmap` getPOSIXTime
                                    let fileName = userid ++ ("_"::Text) ++ (pack tim)
                                    let fnm = localBuildingPath ++ "hsfiles/" ++ fileName ++ ".hs"
                                    writeFile (unpack fnm) thecode
                                    let cmd = ("sh " ++ localBuildingPath ++ "makecontainerandrun.sh " ++ fileName ++ " " ++ localBuildingPath)
                                    (ecd, stdout, stderr) <- readCreateProcessWithExitCode (shell (unpack cmd)) "" 
                                    return (pack stdout, pack stderr, pack $ show ecd, fileName)


-- these need to be fixed and replaced
localBuildingPath = "dockerSandboxes/"
tempDefaultUserId = "user1"::Text

