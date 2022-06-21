module Web.View.Tweets.Index where
import Web.View.Prelude

data IndexView = IndexView { result :: [(Text, Int)]  }

instance View IndexView where
    html IndexView { result } = [hsx|
        <div>
        {forEach result renderTweet}
        </div>
    |]

renderTweet tweet = [hsx|
    <div>{fst tweet}</div>
    <div>{snd tweet}</div>
|]