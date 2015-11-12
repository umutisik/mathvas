module Model.Activity where

import ClassyPrelude.Yesod
import Data.FileEmbed

data Activity = Activity { activityId :: Text
                         , activityTitle :: Text
                         , activityTemplateContent :: Text
                         , hasImageResult :: Bool
                         }

openHaskell :: Activity
openHaskell = Activity { activityId = "openHaskell"
                       , activityTitle = "Free Haskell"
                       , activityTemplateContent = "main = putStrLn \"Hello World!\""::Text
                       , hasImageResult = False
                       }

images :: Activity
images = Activity { activityId = "images"
                       , activityTitle = "Images"
                       , activityTemplateContent = decodeUtf8 $(embedFile "snippet_code_templates/artTemplate.hs")
                       , hasImageResult = True
                       }



allActivities :: [Activity]
allActivities = [openHaskell, images]
activityFromId :: Text -> Activity
activityFromId "openHaskell" = openHaskell
activityFromId "images" = images
