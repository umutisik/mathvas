module Handler.CreateSnippet where

import Import
import Handler.Save

postCreateSnippetR :: Handler Value
postCreateSnippetR = maybeSaveR Nothing
