{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Handler.Alert where

import Import
import Util.Alert (dangerText, successText)

data AlertMessage = AlertMessage {
    message :: Text
} deriving (Show, Generic)

instance FromJSON AlertMessage

postAlertDangerR :: Handler Value
postAlertDangerR = do
    payload <- requireJsonBody :: Handler AlertMessage
    let msg = dangerText $ message payload
    return $ object ["message" .= msg]

postAlertSuccessR :: Handler Value
postAlertSuccessR = do
    payload <- requireJsonBody :: Handler AlertMessage
    let msg = successText $ message payload
    return $ object ["message" .= msg]
