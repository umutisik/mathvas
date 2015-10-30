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
              	                         codeOutput = writeAndRunGHC thecode
              	                     in liftIO (liftM makeMessage $ codeOutput)
              	         _ -> return runError
              
--              return $ case result of 
--              	         Just jas -> (dumdum $ (text jas)) 
--              	         _ -> dddd

runError = object [ "stdout" .= (""::Text), "stderr" .= (""::Text), "error" .= ("run error"::Text) ]         
makeMessage (stdo,stde,excode) = object [ "stdout" .= stdo, "stderr" .= stde, "error" .= excode ]         

-- unnecessary data structure to help parse the json from the request for postrunr              
data FileText = FileText { text :: Text }
instance FromJSON FileText where
 	parseJSON (Object v) = FileText <$> v .: "file"
 	parseJSON _ = mzero

readShell :: Text -> Text -> IO Text
readShell cmd input = let outp = readShell' (unpack cmd) (unpack input)
					  in liftM pack $ outp
						 
readShell' :: String -> String -> IO String
readShell' cmd input = readProcess "bash" ["-c", cmd] input
-- the code runner for now


writeAndRunGHC :: Text -> IO (Text, Text, Text)
writeAndRunGHC thecode =  do tim <- liftM show $ round `fmap` getPOSIXTime
                             let fnm = localBuildingPath ++ tim ++ ".hs"
                             writeFile fnm thecode
                             --x <- readShell ("runghc " ++ (pack fnm)) ""
                             let cmd = ("runghc " ++ (pack fnm))
                             (ecd, stdout, stderr) <- readCreateProcessWithExitCode (shell cmd) "" 
                             return (pack stdout, pack stderr, pack $ show ecd)


localBuildingPath = "/Users/umutisik/Devel/YesodDeneme/_tempbuildghc/"

--	do x <- ((readShell ("ls -al"::Text) (""::Text)))
--              	          return x

