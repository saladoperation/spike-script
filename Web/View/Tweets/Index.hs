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
    <div>Retweets: {snd tweet}</div>
    <blockquote class="twitter-tweet" width="180" height="520"><a href={"https://twitter.com/x/status/" <> fst tweet}></a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
|]