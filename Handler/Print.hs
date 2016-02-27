module Handler.Print where

import Import
import Model.Activity
import Database.Persist.Sql (fromSqlKey, toSqlKey)
import Settings.Environment
import Text.Read (read)

postPrintR :: Handler Html
postPrintR =  do 
    userId <- requireAuthId
    (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
    selected <- runInputPost $ ireq textField "snippetselector"
    mSnippet <- runDB $ get ((toSqlKey $ ((read $ unpack selected):: Int64))::StoredSnippetId)
    case mSnippet of 
    	Nothing      -> error "There was an error retrieving the snippet you selected."
    	Just snippet -> do liftIO $ writePrintRequest selected profile snippet
                           redirect PrintRequestSentR

getPrintRequestSentR :: Handler Html
getPrintRequestSentR = defaultLayout $ do 
    setTitle "Print Request Sent"
    $(widgetFile "print-request-sent")
    
writePrintRequest selected profile snippet = do 
    	printRequestStoragePath' <- printRequestStoragePath
    	let unm = profileUsername profile
    	let fnm = (printRequestStoragePath' ++ unm ++ "_" ++ selected)
    	let activity = activityFromId $ storedSnippetSnippetActivity snippet
    	let enteredCode = storedSnippetSnippetContent snippet
        let fullCode = (activityHiddenCodeAbove activity) ++ enteredCode ++ (activityHiddenCodeBelow activity) 
    	writeFile (unpack $ fnm ++ ".txt") (unpack $ selected 
    		                                  ++ "\n" ++ unm
    		                                  ++ "\n" ++ (storedSnippetSnippetTitle snippet)
    		                                  ++ "\n" ++ (activityTitle activity)
    		                                  ++ "\n\nEntered Code: \n\n" ++ enteredCode)
    	writeFile (unpack $ fnm ++ ".hs") (unpack $ fullCode) 

getPrintR :: Handler Html
getPrintR = do userId <- requireAuthId
               (Entity _ profile) <- runDB $ getBy404 $ UniqueProfile userId
               let userName = profileUsername profile
               snippetList <- runDB $ selectList [StoredSnippetSnippetOwner ==. userId] [Desc StoredSnippetSnippetModified]
               defaultLayout $ do setTitle $ "Mathvas - Print Request Page"
                                  $(widgetFile "print")

