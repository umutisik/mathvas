module Model.Activity where

import ClassyPrelude.Yesod
import Data.FileEmbed
import Data.Text (splitOn)

data Activity = Activity { activityId :: Text
                         , activityTitle :: Text
                         , activityHiddenCodeAbove :: Text
                         , activityStudentCodeDefault :: Text
                         , activityHiddenCodeBelow :: Text
                         , hasImageResult :: Bool
                         } deriving (Show)

openHaskell :: Activity
openHaskell = Activity { activityId = "openHaskell"
                       , activityTitle = "Free Haskell"
                       , activityHiddenCodeBelow = ""
                       , activityHiddenCodeAbove = ""
                       , activityStudentCodeDefault = "main = putStrLn \"Hello World!\""::Text
                       , hasImageResult = False
                       }

images :: Activity
images = let templateFileText = decodeUtf8 $(embedFile "snippet_code_templates/images.hs")
             [abv, stude, blw] = take 3 $ splitOn templateSplitterString templateFileText
         in Activity { activityId = "images"
                       , activityTitle = "Images"
                       , activityHiddenCodeBelow = blw
                       , activityHiddenCodeAbove = abv
                       , activityStudentCodeDefault = stude
                       , hasImageResult = True
                       }



allActivities :: [Activity]
allActivities = [openHaskell, images]
activityFromId :: Text -> Activity
activityFromId "openHaskell" = openHaskell
activityFromId "images" = images

templateSplitterString = "--STUDENTCODEDELIMITER---\n"