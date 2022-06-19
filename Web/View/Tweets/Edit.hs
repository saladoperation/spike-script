module Web.View.Tweets.Edit where
import Web.View.Prelude

data EditView = EditView { tweet :: Tweet }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Tweet</h1>
        {renderForm tweet}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Tweets" TweetsAction
                , breadcrumbText "Edit Tweet"
                ]

renderForm :: Tweet -> Html
renderForm tweet = formFor tweet [hsx|
    
    {submitButton}

|]