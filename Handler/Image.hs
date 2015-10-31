module Handler.Image where

import Import
import Network.Wai (lazyRequestBody)
import Data.FileEmbed (embedFile)

--temp
import Handler.Run (localBuildingPath)

--import Data.Aeson (decode, Object)
--import Data.Aeson.Types (parseMaybe)
--import Data.Time.Clock.POSIX

--import System.Process

getImageR :: Text -> Handler TypedContent
getImageR name = let file = (readFile (unpack (localBuildingPath ++ name)))::(IO ByteString)
                 in liftIO (liftM (TypedContent "image/jpeg" . toContent) $ file)
