# Cortex turn formatting

    Code
      cat(turn@text)
    Output
      This semantic data model...
      
      ```sql
      SELECT SUM(revenue) FROM key_business_metrics
      ```
      
      #### Suggestions
      
      - What is the total quantity sold for each product last quarter?
      - What is the average discount percentage for orders from the United States?
      - What is the average price of products in the 'electronics' category?

---

    Code
      cat(format(turn))
    Output
      This semantic data model...
      SQL: `SELECT SUM(revenue) FROM key_business_metrics`
      Suggestions:
      * What is the total quantity sold for each product last quarter?
      * What is the average discount percentage for orders from the United States?
      * What is the average price of products in the 'electronics' category?

# Cortex API requests are generated correctly

    Code
      req
    Message
      <httr2_request>
      POST
      https://testorg-test_account.snowflakecomputing.com/api/v2/cortex/analyst/message
      Headers:
      * Authorization: '<REDACTED>'
      * X-Snowflake-Authorization-Token-Type: 'OAUTH'
      Body: json encoded data
      Options:
      * timeout_ms: 60000
      * connecttimeout: 0
      * useragent: 'r_elmer/0.0.0.9000'
      Policies:
      * retry_max_tries: 2
      * retry_on_failure: FALSE
      * error_body: a function

---

    Code
      req$body$data
    Output
      $messages
      $messages[[1]]
      $messages[[1]]$role
      [1] "user"
      
      $messages[[1]]$content
      $messages[[1]]$content[[1]]
      $messages[[1]]$content[[1]]$type
      [1] "text"
      
      $messages[[1]]$content[[1]]$text
      [1] "Tell me about my data."
      
      
      
      
      
      $stream
      [1] FALSE
      
      $semantic_model_file
      [1] "@my_db.my_schema.my_stage/model.yaml"
      

# the session parameter is ignored when not on Connect

    Code
      . <- chat_cortex("testorg-test_account", model_file = "model.yaml", session = session)
    Message
      ! Ignoring the `session` parameter.
      i Viewer-based credentials are only available when running on Connect.

# missing viewer credentials generate errors on Connect

    Code
      . <- chat_cortex("testorg-test_account", model_file = "model.yaml", session = session)
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
      [1] "https%3A%2F%2Ftestorg-test_account.snowflakecomputing.com"
      
      

