module Util.Handler (
    urlDecode',
    title,
    titleConcat,
    pageNo
) where

import Import
import Prelude (read)
import Text.Blaze (toMarkup, Markup)

urlDecode' :: Text -> Text
urlDecode' x = decodeUtf8 $ urlDecode True $ encodeUtf8 x

title :: Text -> Markup
title text = toMarkup $ text

titleConcat :: [Text] -> Markup
titleConcat parts = toMarkup $ concat parts

pageNo :: Maybe Text -> Int
pageNo (Just page) = read $ unpack page
pageNo Nothing = 1
