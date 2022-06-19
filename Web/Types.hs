module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data TweetsController
    = TweetsAction
    | NewTweetAction
    | ShowTweetAction { tweetId :: !(Id Tweet) }
    | CreateTweetAction
    | EditTweetAction { tweetId :: !(Id Tweet) }
    | UpdateTweetAction { tweetId :: !(Id Tweet) }
    | DeleteTweetAction { tweetId :: !(Id Tweet) }
    deriving (Eq, Show, Data)
