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
      
      

