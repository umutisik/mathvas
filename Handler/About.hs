module Handler.About where

import Import

getAboutR :: Handler Html
getAboutR = defaultLayout $ do setTitle $ "About Studio Math"
                               $(widgetFile "about")

