module Handler.Print where

import Import
import Model.Activity
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Settings.Environment
import Text.Read (read)
import Data.Aeson
--import qualified Data.ByteString.Lazy as DB (unpack)
import qualified Data.Text.Encoding as DTE (encodeUtf8)

postPrintR :: Handler Html
postPrintR =  do 
    userId <- requireAuthId
    mUser <- runDB $ get userId
    (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
    selected <- runInputPost $ ireq textField "snippetselector"
    mSnippet <- runDB $ get ((toSqlKey $ ((read $ unpack selected):: Int64))::StoredSnippetId)
    case (mSnippet, mUser) of 
    	(Just snippet, Just user) -> do liftIO $ writePrintRequest selected user profile snippet
                                        redirect PrintRequestSentR
        _                         -> error "There was an error retrieving the snippet you selected."                   

getPrintRequestSentR :: Handler Html
getPrintRequestSentR = defaultLayout $ do 
    setTitle "Print Request Sent"
    $(widgetFile "print-request-sent")
    
writePrintRequest selected user profile snippet = do 
    printRequestStoragePath' <- printRequestStoragePath
    let unm = profileUsername profile
    let uemail = userEmail user
    let fnm = (printRequestStoragePath' ++ unm ++ "_" ++ selected)
    let activity = activityFromId $ storedSnippetSnippetActivity snippet
    let enteredCode = storedSnippetSnippetContent snippet
    let fullCode = (activityHiddenCodeAbove activity) ++ "\n--STUDENTCODEDELIMITER---\n" ++ enteredCode ++ "\n--STUDENTCODEDELIMITER---\n" ++ (activityHiddenCodeBelow activity) 
    let jsonToWrite = object [ "username" .= unm
                               , "email" .= uemail
                               , "snippetTitle" .= (storedSnippetSnippetTitle snippet)
                               , "snippetId" .= selected
                               , "activity" .= (activityTitle activity)
                               , "enteredCode" .= enteredCode ]         
    let details = (DTE.encodeUtf8 $ "Username: " ++ unm
                            ++ "\nEmail: " ++ uemail
                            ++ "\nSnippet Title: " ++ (storedSnippetSnippetTitle snippet)
                            ++ "\nSnippet Id: " ++ selected
                            ++ "\nActivity: " ++ (activityTitle activity)
                            ++ "\nEntered Code: \n" ++ enteredCode)
    writeFile (unpack $ fnm ++ ".txt") details
    writeFile (unpack $ fnm ++ ".json") (DTE.encodeUtf8 $ pack $ show jsonToWrite)
    writeFile (unpack $ fnm ++ ".hs") (DTE.encodeUtf8 $ fullCode) 
    adminemailaddress <- adminEmail
    sendEmail' [encodeUtf8 adminemailaddress] "new print request"  details



getPrintR :: Handler Html
getPrintR = do userId <- requireAuthId
               (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
               let userName = profileUsername profile
               snippetList <- runDB $ selectList [StoredSnippetSnippetOwner ==. userId] [Desc StoredSnippetSnippetModified]
               defaultLayout $ do setTitle $ "Mathvas - Print Request Page"
                                  $(widgetFile "print")

