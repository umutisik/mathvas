module Widget.SubContent where

import Import
import Yesod.Markdown
import Text.Julius (rawJS)

showHideWidget :: Int -> Html -> Widget
showHideWidget idno cont = 
	$(widgetFile "widget/showhide")

