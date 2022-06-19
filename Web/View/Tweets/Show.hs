module Web.View.Tweets.Show where
import Web.View.Prelude

data ShowView = ShowView { tweet :: Tweet }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Tweet</h1>
        <p>{tweet}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Tweets" TweetsAction
                            , breadcrumbText "Show Tweet"
                            ]