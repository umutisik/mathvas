module Widget.SubContent where

import Import
import Yesod.Markdown
import Text.Julius (rawJS)

showHideWidget :: Int -> Text -> Html -> Widget
showHideWidget idno label cont = 
	$(widgetFile "widget/showhide")

