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
import qualified Data.Map as Map
import Data.Maybe
import qualified Config


instance Controller TweetsController where
    action TweetsAction = do
        result :: [(Text, Int)] <- sqlQuery [i|
            select t0.id, t0.retweet_count
            from tweets t0
            left outer join tweets t1
            on (t0.id = t1.id and t0.created_at < t1.created_at)
            where t1.id is null
            order by t0.retweet_count desc
        |]  ()
        tweets <- query @Tweet |> fetch
        render IndexView { result }

    action NewTweetAction = do
        let Config.BearerToken bearerToken = getAppConfig @Config.BearerToken
        let Config.Search search = getAppConfig @Config.Search
        let opts = defaults & header "Authorization" .~ [TSE.encodeUtf8 $ "Bearer " <> bearerToken]
        r <- getWith opts $ T.unpack search
        let tweetIds :: [Id Tweet] = map Id $ r ^.. responseBody . key "data" . values . key "id" ._String
        tweets <- query @Tweet
            |> distinctOn #id
            |> fetch
        let existingIds :: [Id Tweet] = map (get #id) tweets

        let newIds = tweetIds \\ existingIds
        let tweets = newIds
                    |> map (\id -> newRecord @Tweet
                                |> set #id id)
        createMany tweets

        tweets <- query @Tweet
            |> distinctOn #id
            |> fetch
        let twitterIds = map (toText . get #id) tweets
        let chunks = chunksOf 100 twitterIds 
        let urls = map (T.unpack . ("https://api.twitter.com/2/tweets?tweet.fields=public_metrics&ids=" <>) . intercalate ",") chunks

        rs <- mapM (getWith opts) urls

        let tweetIds = concatMap (\r -> r ^.. responseBody . key "data" . values . key "id" ._String) rs

        let retweetCounts = concatMap (\r -> r ^.. responseBody . key "data" . values . key "public_metrics" . key "retweet_count" . _Integer
                                                |> map fromIntegral) rs

        result :: [(Text, Int)] <- sqlQuery [i|
            select t0.id, t0.retweet_count
            from tweets t0
            left outer join tweets t1
            on (t0.id = t1.id and t0.created_at < t1.created_at)
            where t1.id is null
            order by t0.retweet_count desc
        |]  ()
        
        let updates = zip tweetIds retweetCounts \\ result

        let tweets = map (\(id, retweetCount) -> newRecord @Tweet
                                                    |> set #id (Id id)
                                                    |> set #retweetCount retweetCount) updates

        createMany tweets

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

toText :: Id Tweet -> Text
toText (Id s) = s