module Handler.Image where

import Import
import Network.Wai (lazyRequestBody)
import Data.FileEmbed (embedFile)

--temp
import Settings.Environment

--import Data.Aeson (decode, Object)
--import Data.Aeson.Types (parseMaybe)
--import Data.Time.Clock.POSIX

--import System.Process

getImageR :: Text -> Handler TypedContent
getImageR name = liftIO $ do localBuildingPath' <- localBuildingPath
                             file <- (readFile (unpack (localBuildingPath' ++ "outputimages/" ++ name)))::(IO ByteString)
                             return ((TypedContent "image/jpeg" . toContent) file)
