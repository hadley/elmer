# defaults are reported

    Code
      . <- chat_databricks()
    Message
      Using model = "databricks-dbrx-instruct".

# M2M authentication requests look correct

    Code
      list(url = req$url, headers = req$headers, body = req$body$data)
    Output
      $url
      [1] "https://example.cloud.databricks.com/oidc/v1/token"
      
      $headers
      $headers$Authorization
      [1] "Basic aWQ6c2VjcmV0"
      
      $headers$Accept
      [1] "application/json"
      
      attr(,"redact")
      [1] "Authorization"
      
      $body
      $body$grant_type
      [1] "client_credentials"
      
      $body$scope
      [1] "all-apis"
      
      

# the session parameter is ignored when not on Connect

    Code
      . <- chat_databricks(session = session)
    Message
      Using model = "databricks-dbrx-instruct".
      ! Ignoring the `session` parameter.
      i Viewer-based credentials are only available when running on Connect.

# missing viewer credentials generate errors on Connect

    Code
      . <- chat_databricks(session = session)
    Message
      Using model = "databricks-dbrx-instruct".
    Condition
      Error in `connectcreds::has_viewer_token()`:
      ! Cannot fetch viewer-based credentials for the current Shiny session.
      Caused by error in `connect_viewer_token()`:
      ! Viewer-based credentials are not supported by this version of Connect.

# token exchange requests to Connect look correct

    Code
      list(url = req$url, headers = req$headers, body = req$body$data)
    Output
      $url
      [1] "localhost:3030/__api__/v1/oauth/integrations/credentials"
      
      $headers
      $headers$Authorization
      [1] "Key key"
      
      $headers$Accept
      [1] "application/json"
      
      attr(,"redact")
      [1] "Authorization"
      
      $body
      $body$grant_type
      [1] "urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Atoken-exchange"
      
      $body$subject_token
      [1] "user-token"
      
      $body$subject_token_type
      [1] "urn%3Aposit%3Aconnect%3Auser-session-token"
      
      $body$resource
      [1] "https%3A%2F%2Fexample.cloud.databricks.com"
      
      

