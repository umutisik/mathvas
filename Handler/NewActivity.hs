module Handler.NewActivity where

import Import
import Widget.Activities

getNewActivityR :: Handler Html
getNewActivityR = do  defaultLayout $ do setTitle "New snippet - Choose language"
                                         $(widgetFile "new")

