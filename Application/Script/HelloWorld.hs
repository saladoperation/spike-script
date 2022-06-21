#!/usr/bin/env run-script
module Application.Script.HelloWorld where

import Application.Script.Prelude
import qualified Data.Text as T
import qualified System.Environment as Environment
import qualified IHP.Log as Log
import qualified Data.Text.Encoding as TSE
import Network.Wreq hiding (get)
import Control.Lens hiding ((|>), set)
import Data.Aeson.Lens
import Data.List.Split (chunksOf)
import Data.String.Interpolate.IsString (i)
import qualified Data.Map as Map
import Data.Maybe

run :: Script
run = do
    bearerToken <- T.pack <$> Environment.getEnv "BEARER_TOKEN"
    let opts = defaults & header "Authorization" .~ [TSE.encodeUtf8 $ "Bearer " <> bearerToken]
    r <- getWith opts "https://api.twitter.com/2/tweets/search/recent?query=-is%3Aretweet%0Ahas%3Aimages%0A%22%23Studio%22%0A%22%40worker99371032%22&sort_order=recency"
    let tweetIds = r ^.. responseBody . key "data" . values . key "id" ._String
    tweets <- query @Tweet
            |> filterWhereIn (#tweetId, tweetIds)
            |> fetch
    let existingIds = map (get #tweetId) tweets
    let newIds = tweetIds \\ existingIds
    let tweets = newIds
                |> map (\tweetId -> newRecord @Tweet
                            |> set #tweetId tweetId)
    createMany tweets

    tweets <- query @Tweet |> fetch
    let twitterIds = map (get #tweetId) tweets
    let chunks = chunksOf 100 twitterIds 
    let urls = map (T.unpack . ("https://api.twitter.com/2/tweets?tweet.fields=public_metrics&ids=" <>) . intercalate ",") chunks

    rs <- mapM (getWith opts) urls

    let tweetIds = concatMap (\r -> r ^.. responseBody . key "data" . values . key "id" ._String) rs

    let retweetCounts = concatMap (\r -> r ^.. responseBody . key "data" . values . key "public_metrics" . key "retweet_count" . _Integer
                                            |> map fromIntegral) rs

    result :: [(Text, Int)] <- sqlQuery [i|
        select tweets.tweet_id, t0.retweet_count
        from metrics t0
        left outer join metrics t1
        on (t0.id = t1.id and t0.created_at < t1.created_at)
        inner join tweets
        on (t0.tweet_id = tweets.id)
        where t1.id is null
    |]  ()

    let updates = zip tweetIds retweetCounts \\ result

    let dictionary = tweets 
                    |> map (\tweet -> (get #tweetId tweet, get #id tweet))
                    |> Map.fromList

    let fromTwitterId twitterId = fromJust $ Map.lookup twitterId dictionary 

    let metrics = map (\(id, retweetCount) -> newRecord @Metric
                                                |> set #tweetId (fromTwitterId id)
                                                |> set #retweetCount retweetCount) updates

    createMany metrics

    pure ()
