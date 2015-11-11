{-# LANGUAGE DeriveGeneric #-}
module Handler.Account where

import Import
import Yesod.Auth.Simple (setPasswordR)
import Util.Slug (mkSlug)
import Util.User (newToken)
import Util.Handler (title)
import Util.Alert (successHtml)

data ProfileData = ProfileData {
    name :: Text,
    username :: Text
} deriving (Show, Generic)

instance FromJSON ProfileData

getAccountProfileR :: Handler Html
getAccountProfileR = do
    userId <- requireAuthId
    Entity _ profile <- runDB $ getBy404 $ UniqueProfile userId
    defaultLayout $ do
        setTitle $ title "Profile"
        $(widgetFile "account/profile")

putAccountProfileR :: Handler Value
putAccountProfileR = do
    userId <- requireAuthId
    profileData <- requireJsonBody :: Handler ProfileData
    Entity profileId _ <- runDB $ getBy404 $ UniqueProfile userId
    now <- liftIO getCurrentTime
    runDB $ update profileId [
        ProfileName =. name profileData,
        ProfileUsername =. (mkSlug $ username profileData),
        ProfileModified =. now]
    setMessage $ successHtml "Profile updated"
    return $ object []
