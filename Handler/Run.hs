module Handler.Run where

import Import
import Network.Wai (lazyRequestBody)
import Data.Aeson (decode, Object)
import Data.Aeson.Types (parseMaybe)

import System.Process

postRunR :: Handler Value
postRunR = do req <- reqWaiRequest <$> getRequest
              body <- liftIO $ lazyRequestBody req
              let bdson = (decode body)::(Maybe FileText)
              case bdson of               
              	         Just jas -> let thecode = text jas 
              	                         codeOutput = writeAndRunGHC thecode
              	                     in liftIO (liftM dumdum $ codeOutput)
              	         _ -> return dddd
              
--              return $ case result of 
--              	         Just jas -> (dumdum $ (text jas)) 
--              	         _ -> dddd


dddd = object [ "stdout" .= (""::Text), "stderr" .= (""::Text), "error" .= ("run error"::Text) ]         
dumdum contt = object [ "stdout" .= contt, "stderr" .= (""::Text), "error" .= ("run error"::Text) ]         

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


writeAndRunGHC :: Text -> IO Text
writeAndRunGHC thecode =  do writeFile ("/Users/umutisik/Devel/YesodDeneme/_tempbuildghc/temp.hs") thecode
                             x <- (readShell "runghc /Users/umutisik/Devel/YesodDeneme/_tempbuildghc/temp.hs" "")
                             return x


--	do x <- ((readShell ("ls -al"::Text) (""::Text)))
--              	          return x

