module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig

newtype BearerToken = BearerToken Text

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    bearerToken <- BearerToken <$> env @Text "BEARER_TOKEN"
    option bearerToken