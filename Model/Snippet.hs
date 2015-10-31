module Model.Snippet (
    Snippet(..),
    defaultSnippet,
) where

import ClassyPrelude.Yesod

data Snippet = Snippet {
    snippetId :: Text,
    snippetTitle :: Text,
    snippetOwner :: Text,
    snippetContent :: Text
} deriving (Show)



defaultSnippet :: Snippet
defaultSnippet = helloWorldSnippet

helloWorldSnippet :: Snippet
helloWorldSnippet = Snippet{
                        snippetId="",
                        snippetTitle="Untitled",
                        snippetOwner="",
                        snippetContent= "main = putStrLn \"Hello World!\""
                    }
