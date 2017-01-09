module Handler.WellKnown where

import Import
import Settings.Environment
import Data.Char

getWellKnownR :: Text -> Handler TypedContent
getWellKnownR text = liftIO $ do let safename = filter (\x -> isAlphaNum x || ((==) '-' x) || ((==) '/' x)) text
                                 --let safename = text
                                 let path = "/var/.well-known/acme-challenge/" ++ safename
                                 file <- (readFile (unpack (path)))::(IO ByteString)
                                 return ((TypedContent "text" . toContent) file)
