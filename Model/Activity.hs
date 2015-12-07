module Model.Activity where

import ClassyPrelude.Yesod
import Data.FileEmbed
import Data.Text (splitOn, count)

data Activity = Activity { activityId :: Text
                         , activityTitle :: Text
                         , activityHiddenCodeAbove :: Text
                         , activityStudentCodeDefault :: Text
                         , activityHiddenCodeBelow :: Text
                         , hasImageResult :: Bool
                         , firstLineNumber :: Int
                         } deriving (Show)

-- Pre-defined activities

openHaskell :: Activity
openHaskell = Activity { activityId = "openHaskell"
                       , activityTitle = "Free Haskell"
                       , activityHiddenCodeBelow = ""
                       , activityHiddenCodeAbove = ""
                       , activityStudentCodeDefault = "main = putStrLn \"Hello World!\""::Text
                       , hasImageResult = False
                       , firstLineNumber = 1
                       }

evaluate :: Activity
evaluate = Activity { activityId = "evaluate"
                       , activityTitle = "Evaluate"
                       , activityHiddenCodeBelow = ""
                       , activityHiddenCodeAbove = "main = putStrLn $ show valueToPrint\n"
                       , activityStudentCodeDefault = "valueToPrint = f 1\n\nf x = x"::Text
                       , hasImageResult = False
                       , firstLineNumber = 1
                       }

graph :: Activity
graph = let templateFileText = decodeUtf8 $(embedFile "snippet_code_templates/graph.hs")
            [abv, stude, blw] = take 3 $ splitOn templateSplitterString templateFileText
         in Activity { activityId = "graph"
                       , activityTitle = "Graph"
                       , activityHiddenCodeBelow = blw
                       , activityHiddenCodeAbove = abv
                       , activityStudentCodeDefault = stude
                       , hasImageResult = True
                       , firstLineNumber = (Data.Text.count "\n" abv) + 1
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
                       , firstLineNumber = (Data.Text.count "\n" abv) + 1
                       }

grayscale :: Activity
grayscale = let templateFileText = decodeUtf8 $(embedFile "snippet_code_templates/grayscale.hs")
                [abv, stude, blw] = take 3 $ splitOn templateSplitterString templateFileText
            in Activity { activityId = "grayscale"
                       , activityTitle = "Grayscale"
                       , activityHiddenCodeBelow = blw
                       , activityHiddenCodeAbove = abv
                       , activityStudentCodeDefault = stude
                       , hasImageResult = True
                       , firstLineNumber = (Data.Text.count "\n" abv) + 1
                       }



allActivities :: [Activity]
allActivities = [evaluate, graph, grayscale, images, openHaskell]
activityFromId :: Text -> Activity
activityFromId "openHaskell" = openHaskell
activityFromId "images" = images
activityFromId "grayscale" = grayscale
activityFromId "evaluate" = evaluate
activityFromId "graph" = graph
--activityFromId _ = openHaskell

templateSplitterString = "--STUDENTCODEDELIMITER---\n"