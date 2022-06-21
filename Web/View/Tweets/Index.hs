module Web.View.Tweets.Index where
import Web.View.Prelude

data IndexView = IndexView { result :: [(Text, Int)]  }

instance View IndexView where
    html IndexView { result } = [hsx|
        <div class="d-flex flex-wrap">
        {forEach result renderTweet}
        </div>
    |]

renderTweet tweet = [hsx|
    <div>
        <div>Retweets: {snd tweet}</div>
        <div>
        <blockquote class="twitter-tweet"><a href={"https://twitter.com/x/status/" <> fst tweet}></a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>
        </div>
    </div>
|]