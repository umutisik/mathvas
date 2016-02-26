module Widget.Editor (
    editorWidget
) where

import Import
import Model.Snippet
import Model.Activity
import Text.Julius (rawJS)

editorWidget :: Snippet -> Int -> Widget
editorWidget snippet numberOfLines = do
    addScript $ StaticR lib_ace_ace_js
    $(widgetFile "editor")

settingsWidget :: Snippet -> Widget
settingsWidget snippet = let activity = snippetActivity snippet
                         in $(widgetFile "widget/settings")

