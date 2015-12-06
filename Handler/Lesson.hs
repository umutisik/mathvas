module Handler.Lesson where

import Import
import Model.Activity
import Model.Snippet
import Widget.Editor
import Widget.RunResult
import Text.Julius (rawJS)
import Database.Persist.Sql (toSqlKey)
import Settings.Environment
import Util.Util
import Yesod.Markdown
import Text.Pandoc



getLessonR :: Text -> Handler Html
getLessonR lsntitle = do
    userId <- requireAuthId
    let handlerName = "getLessonR" :: Text
    _ <- liftIO $ do existo <- liftM (elem lsntitle) allLessonNames
                     when (existo == False) $ error "Error. Couldn't find lesson. Is this a lesson from the list of lessons?"                  
    defaultLayout $ do
        -- addScript $ StaticR lib_ace_ace_js 
        addStylesheet $ StaticR css_markdown_css
        addScriptRemote mathJaxJsUrl
        setTitle "Studio Math!"
        $(widgetFile "homepage")
        mm2 <- let fpth = ((lessonsPath::FilePath) ++ ((unpack lsntitle)::FilePath) ++ ".md") 
               in liftIO $ fmap markdownToHtml' (markdownFromFile fpth) 
        case mm2 of 
            Left _ -> error "Error. Could not find or process the lesson file."
            Right cont -> $(widgetFile "widget/lessoncontent")
        

markdownToHtml' = fmap (writePandoc markdownWriterOptions)
                         . parseMarkdown yesodDefaultReaderOptions

markdownWriterOptions = def
  { writerHtml5     = True
  , writerWrapText  = False
  , writerHighlight = True
  , writerHTMLMathMethod = MathJax mathJaxJsUrl
  }

mathJaxJsUrl = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"


allLessonNames :: IO [Text]
allLessonNames = (liftM (map fst)) (parseLessonList ((lessonsPath ++ "lesson_list")::FilePath)) 











        --let mm = Markdown $ ("# Hellodeneme\n This is some markdown \n\n Yes. And here is the editorr"::Text)
        --[whamlet| #{mm} |]
        --let numberOfLines = 14
        --    sId = toSqlKey 0
        --    snippetId = ("" :: Text)
        --    activityName = "images"::Text
        --    isExistingSnippet = False
        --    activity = activityFromId activityName
        --    snippet = (defaultSnippet activity)
        -- $(widgetFile "compose") 
        -- $(widgetFile "compose") 
        --[whamlet| #{mm} |]
        --editorWidget (defaultSnippet $ activityFromId "openHaskell") 10
        --editorWidget (defaultSnippet $ activityFromId "openHaskell") 10
        --        -- for all editors to work, they need to have different id numbers
        --        -- what i need here is a custom small-compose widget
        --        -- probably for now, best to keep to single editor though and just focus on the markup
        --        -- another caveat: if you need extra hidden haskell code, then how is RUN going to learn about it?
