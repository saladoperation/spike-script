module Web.View.Tweets.New where
import Web.View.Prelude

data NewView = NewView { tweet :: Tweet }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Tweet</h1>
        {renderForm tweet}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Tweets" TweetsAction
                , breadcrumbText "New Tweet"
                ]

renderForm :: Tweet -> Html
renderForm tweet = formFor tweet [hsx|
    
    {submitButton}

|]