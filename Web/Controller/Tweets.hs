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
import Network.Wreq
import Control.Lens hiding ((|>), set)
import Data.Aeson.Lens

instance Controller TweetsController where
    action TweetsAction = do
        bearerToken <- T.pack <$> Environment.getEnv "BEARER_TOKEN"
        let opts = defaults & header "Authorization" .~ [TSE.encodeUtf8 $ "Bearer " <> bearerToken]
        r <- getWith opts "https://api.twitter.com/2/tweets?ids=1538335385163427841&tweet.fields=public_metrics&expansions=attachments.media_keys&media.fields=url"
        case r ^? responseBody . key "includes" . key "media" . nth 0 . key "url" ._String of
            Just url -> Log.info url
            Nothing -> pure ()
        tweets <- query @Tweet |> fetch
        render IndexView { .. }

    action NewTweetAction = do
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
