module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig

newtype BearerToken = BearerToken Text
newtype Search = Search Text

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")


    search <- Search <$> envOrDefault "SEARCH" "https://api.twitter.com/2/tweets/search/recent?query=-is%3Aretweet%0Ahas%3Aimages%0A%22%23Studio%22%0A%22%40worker99371032%22&sort_order=recency"
    option search
    bearerToken <- BearerToken <$> env "BEARER_TOKEN"
    option bearerToken