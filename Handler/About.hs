module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayout $ do setTitle $ "About Mathvas"
                               $(widgetFile "about")

