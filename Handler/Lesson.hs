module Handler.Lesson where

import Import
import Model.Activity
import Model.Snippet
import Widget.Editor
import Widget.RunResult
import Widget.SubContent
import Text.Julius (rawJS)
import Data.Text (splitOn)
import Data.List (head)
import Database.Persist.Sql (toSqlKey)
import Settings.Environment
import Util.Util
import Yesod.Markdown

import Text.Pandoc
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesFileExist)

import Text.ParserCombinators.Parsec as P


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
        lessonsPath' <- liftIO lessonsPath
        let fpth = (((unpack lessonsPath')::FilePath) ++ ((unpack lsntitle)::FilePath) ++ ".md") 
        let listo = (markdownListFromFile fpth) 
        lsmm <- liftIO $ do lst <- listo
                            let htls = [ (a, (markdownToHtml') b)  | (a,b) <- lst ]
                            return $ [ (a,b,c) | (a,(b,c))  <- (zip [1,2..] htls) ]

        let handl xxx = case xxx of 
                          (_,_,Left _) -> error "Error. Could not find or process the lesson file."
                          (_,Normal,Right cont) -> $(widgetFile "widget/lessoncontent")
                          (idno,ShowHide, Right cont) -> showHideWidget idno cont

        sequence_ $ map handl lsmm


markdownToHtml' = fmap (writePandoc markdownWriterOptions)
                         . parseMarkdown yesodDefaultReaderOptions

markdownWriterOptions = def
  { writerHtml5     = True
  , writerWrapText  = False
  , writerHighlight = True
  , writerHTMLMathMethod = MathJax mathJaxJsUrl
  }

mathJaxJsUrl = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"

data SubContentType = Normal | ShowHide | ActiveCode

-- | Returns the empty string if the file does not exist
markdownListFromFile :: FilePath -> IO [(SubContentType,Markdown)]
markdownListFromFile f = do
    exists  <- doesFileExist f
    content <-
        if exists
            then readFileUtf8 f
            else return ""

    return $ parseLesson content

    where
        readFileUtf8 :: FilePath -> IO Text
        readFileUtf8 fp = do
            bs <- B.readFile fp
            return $ decodeUtf8With lenientDecode bs

parseLesson :: Text -> [(SubContentType,Markdown)]
parseLesson inp =  case ((parse lessonParser "(unknown)" $ unpack inp)) of
                         Right out -> out
                         Left e    -> error $ "parse error in lesson file: \n" ++ (show e)

lessonParser :: GenParser Char st [(SubContentType,Markdown)]
lessonParser = do result <- manyTill ((P.try showHide) <||> normal) eof
                  return result


showHide :: GenParser Char st (SubContentType,Markdown)
showHide = do string "@@@"
              tcon <- manyTill anyChar (P.try (string "@@@"))
              return $ (ShowHide,Markdown $ pack (tcon))
                     
normal :: GenParser Char st (SubContentType,Markdown)
normal = do tcon <- (P.try (manyTill anyChar (lookAhead $ P.try (string "@@@"))) <||> (P.many P.anyChar))
            return $ (Normal, Markdown $ pack tcon)

allLessonNames :: IO [Text]
allLessonNames = do lessonsPath' <- lessonsPath
                    (liftM (map fst)) (parseLessonList ((unpack lessonsPath' ++ "lesson_list")::FilePath)) 











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
