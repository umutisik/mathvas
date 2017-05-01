module Settings.Environment where

import ClassyPrelude.Yesod
import System.Environment (getEnv,lookupEnv)
import Text.Read (read)
-- lookupEnv


-- email
mailgunDomain :: IO String
mailgunDomain = getEnv "MAILGUN_DOMAIN"

mailgunApiKey :: IO String
mailgunApiKey = getEnv "MAILGUN_API_KEY"

emailFromAddress :: IO Text
emailFromAddress = pack <$> getEnv "EMAIL_FROM_ADDRESS"

adminEmail :: IO Text
--adminEmail = "umutisik@gmail.com"
adminEmail = pack <$> getEnv "ADMIN_EMAIL"


-- local paths 

lessonsPath :: IO Text
--lessonsPath = "Lessons/"
lessonsPath = pack <$> getEnv "LESSONS_PATH"

localBuildingPath :: IO Text
--localBuildingPath = "dockerSandboxes/"
localBuildingPath = pack <$> getEnv "LOCAL_BUILDING_PATH"

imageStoragePath :: IO Text
imageStoragePath = liftM (flip (++) "outputimages/") $ localBuildingPath 

printRequestStoragePath :: IO Text
printRequestStoragePath = liftM (flip (++) "print-requests/") $ localBuildingPath 

openRegistration :: IO Bool
openRegistration = (== "True") <$> getEnv "OPEN_REGISTRATION"

tempDefaultUserId :: Text
tempDefaultUserId = "usertemp"

-- in microseconds
globalTimeLimitOnRuns :: IO Int
--globalTimeLimitOnRuns = 60*1000000 
globalTimeLimitOnRuns = read <$> getEnv "CONTAINER_TIME_LIMIT"

