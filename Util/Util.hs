module Util.Util where

import Import
import Text.ParserCombinators.Parsec

parseLessonList :: FilePath -> IO [(Text,Text)]
parseLessonList fileName = do 
                              inp <- liftM pack $ readFile fileName
                              return $ case (parse lessonListParser "(unknown)" inp) of
                              	         Right lst -> lst
                              	         Left e    -> error $ "error parsing the lesson list" ++ (show e)

--parseLessonList t = return [("Lesson1name asdf asdf asdf ashdf kahsd fkahs dkjfhajksdf jk ashdf "::Text, "Lesson 1 asdfasdf askdjf klasjd flkajs dlkfjals kjfd lkasjd fl Title"::Text),("Lesson1name"::Text, "Lesson 1 Title"::Text),("Lesson1name"::Text, "Lesson 1ajs dfjklaj sdfklja slkdfj laksjd f Title"::Text)]
--parse csvFile "(unknown)" input

lessonListParser :: GenParser Char st [(Text,Text)]
lessonListParser = 
    do result <- many line
       eof
       return result

line :: GenParser Char st (Text,Text)
line  = 
    do manyTill anyChar (char '\"')
       e1 <- manyTill anyChar (char '\"')
       manyTill anyChar (char '\"')
       e2 <- manyTill anyChar (char '\"')
       eol
       return (pack e1, pack e2)

eol :: GenParser Char st Char
eol = char '\n'

  

