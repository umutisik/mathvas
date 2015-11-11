module Settings.Environment (
    mandrillToken,
) where

import ClassyPrelude.Yesod
import System.Environment (getEnv,)
-- lookupEnv

mandrillToken :: IO Text
mandrillToken = return ("XA-x_poftOTL11S-tei3bg"::Text)
--mandrillToken = pack <$> getEnv "MANDRILL_TOKEN"
