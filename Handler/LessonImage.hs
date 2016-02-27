module Handler.LessonImage where

import Import
import Settings.Environment
import Data.Char

--getImageR name = 
getLessonImageR :: Text -> Handler TypedContent
getLessonImageR name = liftIO $ do lessonsPath' <- lessonsPath
                                   let safename = filter (\x -> isAlphaNum x || ((==) '.' x) || ((==) '_' x)) name
                                   file <- (readFile (unpack (lessonsPath' ++ "images/" ++ safename)))::(IO ByteString)
                                   return ((TypedContent "image/jpeg" . toContent) file)



