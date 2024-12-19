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
      * useragent: 'r_ellmer/0.0.0.9000'
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
      

