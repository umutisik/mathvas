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

--getImageR :: Text -> Handler TypedContent
--getImageR name = liftIO $ do localBuildingPath' <- localBuildingPath
--                             file <- (readFile (unpack (localBuildingPath' ++ "outputimages/" ++ name)))::(IO ByteString)
--                             return ((TypedContent "image/jpeg" . toContent) file)

getImageR :: ImageId -> Handler TypedContent
getImageR imId =          do  mUserId <- maybeAuthId
                              mImgProfile <- runDB $ get imId
                              case mImgProfile of
                              	Nothing          -> error "Image does not exist"
                              	Just imgProfile  -> liftIO $ do let filename = imageFileName imgProfile
                              	                                let owner = imageOwner imgProfile
                              	                                let public = imagePublic imgProfile
                              	                                case (public || (hasOwnership mUserId owner)) of
                              	                                    False -> error "Permissions issue when accessing image" 
                                                                    True -> do imageStoragePath' <- imageStoragePath 
                                                                               file <- (readFile (unpack (imageStoragePath' ++ filename)))::(IO ByteString)
                                                                               return ((TypedContent "image/jpeg" . toContent) file)

hasOwnership mUserId owner = case mUserId of
	                             Just uid -> (owner == uid)
	                             Nothing  -> False