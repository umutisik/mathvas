module Handler.Compose where

import Import
import Model.Activity
import Model.Snippet
import Widget.Editor
import Widget.RunResult


getComposeR :: Text -> Handler Html
getComposeR activityName = do 
	let handlerName = "getHomeR" :: Text
	let activity = activityFromId activityName
	defaultLayout $ do
        aDomId <- newIdent
        addScript $ StaticR lib_ace_ace_js 
        setTitle "Studio Math!"
        let snippet = defaultSnippet $ activityFromId activityName
        $(widgetFile "homepage")
        $(widgetFile "compose")

postComposeR :: Text -> Handler Html
postComposeR activity = error "Not yet implemented: postComposeR"


-- snippet <- (liftM newSnippetWithContent) (readFile "snippet_code_templates/artTemplate.hs")

--getHomeR = do
--    (formWidget, formEnctype) <- generateFormPost sampleForm
--    let submission = Nothing :: Maybe (FileInfo, Text)
--        handlerName = "getHomeR" :: Text
--    defaultLayout $ do
--        aDomId <- newIdent
--        addScript $ StaticR lib_ace_ace_js 
--        setTitle "Studio Math!"
--        snippet <- (liftM newSnippetWithContent) (readFile "snippet_code_templates/artTemplate.hs")
--        $(widgetFile "homepage")
--        $(widgetFile "compose")
        
