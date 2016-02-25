module Settings.Environment where

import ClassyPrelude.Yesod
import System.Environment (getEnv,lookupEnv)
import Text.Read (read)
-- lookupEnv

mandrillToken :: IO Text
--mandrillToken = return ("XA-x_poftOTL11S-tei3bg"::Text)
mandrillToken = pack <$> getEnv "MANDRILL_TOKEN"

lessonsPath :: IO Text
--lessonsPath = "Lessons/"
lessonsPath = pack <$> getEnv "LESSONS_PATH"

openRegistration :: IO Bool
--openRegistration = False
openRegistration = (== "True") <$> getEnv "OPEN_REGISTRATION"

adminEmail :: IO Text
--adminEmail = "umutisik@gmail.com"
adminEmail = pack <$> getEnv "ADMIN_EMAIL"

localBuildingPath :: IO Text
--localBuildingPath = "dockerSandboxes/"
localBuildingPath = pack <$> getEnv "LOCAL_BUILDING_PATH"

imageStoragePath :: IO Text
imageStoragePath = liftM (flip (++) "outputimages/") $ localBuildingPath 

tempDefaultUserId :: Text
tempDefaultUserId = "usertemp"

-- in microseconds
globalTimeLimitOnRuns :: IO Int
--globalTimeLimitOnRuns = 60*1000000 
globalTimeLimitOnRuns = read <$> getEnv "CONTAINER_TIME_LIMIT"

