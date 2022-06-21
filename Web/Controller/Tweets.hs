module Web.Controller.Tweets where

import Web.Controller.Prelude
import Web.View.Tweets.Index
import Web.View.Tweets.New
import Web.View.Tweets.Edit
import Web.View.Tweets.Show
import qualified Data.Text as T
import qualified System.Environment as Environment
import qualified IHP.Log as Log
import qualified Data.Text.Encoding as TSE
import Network.Wreq hiding (get)
import Control.Lens hiding ((|>), set)
import Data.Aeson.Lens
import Data.List.Split (chunksOf)
import Data.String.Interpolate.IsString (i)

instance Controller TweetsController where
    action TweetsAction = do
        tweets <- query @Tweet |> fetch
        render IndexView { .. }

    action NewTweetAction = do
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

        let metrics = tweets
                    |> map (\tweet -> newRecord @Metric
                                |> set #tweetId (get #id tweet))
        
        createMany metrics

        tweets <- query @Tweet |> fetch
        let twitterIds = map (get #tweetId) tweets
        let chunks = chunksOf 100 twitterIds 
        let urls = map (T.unpack . ("https://api.twitter.com/2/tweets?tweet.fields=public_metrics&ids=" <>) . intercalate ",") chunks

        rs <- mapM (getWith opts) urls

        let tweetIds = concatMap (\r -> r ^.. responseBody . key "data" . values . key "id" ._String) rs

        let retweetCounts = concatMap (\r -> r ^.. responseBody . key "data" . values . key "public_metrics" . key "retweet_count" . _Integer) rs


        -- result :: [(Id Tweet, Int)] <- sqlQuery "SELECT t0.tweet_id, t0.retweet_count FROM metrics t0 LEFT OUTER JOIN metrics t1 ON (t0.id = t1.id AND t0.retweet_count < t1.retweet_count) WHERE t1.id IS NULL" ()

        result :: [(Id Tweet, Text, Int)] <- sqlQuery [i|
            select t0.tweet_id, tweets.id, t0.retweet_count
            from metrics t0
            left outer join metrics t1
            on (t0.id = t1.id and t0.retweet_count < t1.retweet_count)
            inner join tweets
            on (t0.id = tweets.id)
            where t1.id is null
        |]  ()


        -- result :: [(Int, Int)] <- sqlQuery "SELECT retweet_count, retweet_count FROM metrics" ()

        -- zip (,) tweetIds retweetCounts

        

        let tweet = newRecord
        render NewView { .. }

    action ShowTweetAction { tweetId } = do
        tweet <- fetch tweetId
        render ShowView { .. }

    action EditTweetAction { tweetId } = do
        tweet <- fetch tweetId
        render EditView { .. }

    action UpdateTweetAction { tweetId } = do
        tweet <- fetch tweetId
        tweet
            |> buildTweet
            |> ifValid \case
                Left tweet -> render EditView { .. }
                Right tweet -> do
                    tweet <- tweet |> updateRecord
                    setSuccessMessage "Tweet updated"
                    redirectTo EditTweetAction { .. }

    action CreateTweetAction = do
        let tweet = newRecord @Tweet
        tweet
            |> buildTweet
            |> ifValid \case
                Left tweet -> render NewView { .. } 
                Right tweet -> do
                    tweet <- tweet |> createRecord
                    setSuccessMessage "Tweet created"
                    redirectTo TweetsAction

    action DeleteTweetAction { tweetId } = do
        tweet <- fetch tweetId
        deleteRecord tweet
        setSuccessMessage "Tweet deleted"
        redirectTo TweetsAction

buildTweet tweet = tweet
    |> fill @'[]
