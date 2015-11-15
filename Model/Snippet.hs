module Model.Snippet (
    Snippet(..),
    defaultSnippet,
    snippetContentHash,
    contentHash
) where


import ClassyPrelude.Yesod
import Util.Hash (sha1Text)
import Model.Activity
--import Data.Aeson (decode)
--import Data.Maybe (fromJust)
--import qualified Data.ByteString.Lazy as L


data Snippet = Snippet {
    snippetId :: Text,
    snippetActivity :: Text,
    snippetTitle :: Text,
    snippetPublic :: Bool,
    snippetOwner :: Text,
    snippetHash :: Text,
    snippetModified :: Text,
    snippetCreated :: Text,
    snippetContent :: Text
} deriving (Show)


defaultSnippet :: Activity -> Snippet
defaultSnippet ac = Snippet { snippetId=""
                            , snippetActivity = activityId ac
                            , snippetTitle="Untitled"
                            , snippetPublic=False
                            , snippetOwner=""
                            , snippetHash = contentHash cont
                            , snippetModified=""
                            , snippetCreated=""
                            , snippetContent= cont
                            }
                      where cont = activityStudentCodeDefault ac


--helloWorldSnippet :: Snippet
--helloWorldSnippet = let sniptxt = "main = putStrLn \"Hello World!\""::Text
--                    in newSnippetWithContent sniptxt


--instance FromJSON SnippetFile where
--    parseJSON (Object v) = SnippetFile <$>
--        v .: "name" <*>
--        v .: "content"
--    parseJSON _ = mzero

snippetContentHash :: Snippet -> Text
snippetContentHash s = contentHash $ snippetContent s

contentHash :: Text -> Text
contentHash tt = sha1Text tt
