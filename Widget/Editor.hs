module Widget.Editor (
    editorWidget
) where

import Import
import Model.Snippet 

editorWidget :: Snippet -> Widget
editorWidget snippet = do
    addScript $ StaticR lib_ace_ace_js
    $(widgetFile "editor")

