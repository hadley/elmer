sample_cortex_message <- list(
  role = "analyst",
  content = list(
    list(type = "text", text = "This semantic data model..."),
    list(
      type = "sql",
      statement = "SELECT SUM(revenue) FROM key_business_metrics"
    ),
    list(
      type = "suggestions",
      suggestions = list(
        "What is the total quantity sold for each product last quarter?",
        "What is the average discount percentage for orders from the United States?",
        "What is the average price of products in the 'electronics' category?"
      )
    )
  )
)

test_that("Cortex messages are converted to turns correctly", {
  p <- ProviderCortex(
    account = "testorg-test_account",
    credentials = function(account) list()
  )

  expect_equal(
    # Tests roundtrip conversion.
    as_json(p, value_turn(p, sample_cortex_message)),
    sample_cortex_message
  )
})

test_that("Cortex turn formatting", {
  p <- ProviderCortex(
    account = "testorg-test_account",
    credentials = function(account) list()
  )
  turn <- value_turn(p, sample_cortex_message)
  expect_snapshot(cat(turn@text))
  expect_snapshot(cat(format(turn)))
})

test_that("Cortex chunks are converted to messages correctly", {
  chunks <- list(
    list(
      status = "interpreting_question",
      status_message = "Interpreting question"
    ),
    list(
      type = "text",
      text_delta = "This semantic data model...",
      index = 0
    ),
    list(
      status = "generating_sql",
      status_message = "Generating SQL"
    ),
    list(
      type = "sql",
      statement_delta = "SELECT SUM(revenue) FROM key_business_metrics",
      index = 1
    ),
    list(
      status = "generating_suggestions",
      status_message = "Generating suggestions"
    ),
    list(
      type = "suggestions",
      suggestions_delta = list(
        index = 0,
        suggestion_delta = "What is the total quantity sold for each product last quarter?"
      ),
      index = 2
    ),
    list(
      type = "suggestions",
      suggestions_delta = list(
        index = 1,
        suggestion_delta = "What is the average discount percentage for orders from the United States?"
      ),
      index = 2
    ),
    list(
      type = "suggestions",
      suggestions_delta = list(
        index = 2,
        suggestion_delta = "What is the average price of products in the 'electronics' category?"
      ),
      index = 2
    )
  )
  p <- ProviderCortex(
    account = "testorg-test_account",
    credentials = function(account) list()
  )
  result <- NULL
  output <- ""
  for (chunk in chunks) {
    result <- stream_merge_chunks(p, result, chunk)
    output <- paste0(output, stream_text(p, chunk))
  }
  turn <- value_turn(p, result)
  expect_equal(result, sample_cortex_message$content)
  expect_equal(as_json(p, turn), sample_cortex_message)
  # Make sure streaming output matches batch output.
  expect_equal(output, turn@text)
})

test_that("Cortex API requests are generated correctly", {
  turn <- Turn(
    role = "user",
    contents = list(
      ContentText("Tell me about my data.")
    )
  )
  p <- ProviderCortex(
    account = "testorg-test_account",
    credentials = function(account) list(
      Authorization = paste("Bearer", "obfuscated"),
      `X-Snowflake-Authorization-Token-Type` = "OAUTH"
    ),
    model_file = "@my_db.my_schema.my_stage/model.yaml"
  )
  req <- chat_request(p, FALSE, list(turn))
  expect_snapshot(req)
  expect_snapshot(req$body$data)
})

test_that("a simple Cortex chatbot works", {
  skip_if(
    Sys.getenv("SNOWFLAKE_ACCOUNT") == "",
    "SNOWFLAKE_ACCOUNT is not configured"
  )
  chat <- chat_cortex(
    model_spec = "name: empty
description: An empty semantic model specification.
tables: []
verified_queries: []
"
  )
  resp <- chat$chat("What questions can I ask?")
  # Note: It may not be 100 percent certain this will be in the output.
  expect_match(resp, "semantic data model")
})

# Auth --------------------------------------------------------------------

test_that("the session parameter is ignored when not on Connect", {
  session <- structure(list(request = list()), class = "ShinySession")
  expect_snapshot(. <- chat_cortex(
    "testorg-test_account",
    model_file = "model.yaml",
    session = session
  ))
})

test_that("missing viewer credentials generate errors on Connect", {
  # Mock a Connect environment that *does not* support viewer-based credentials.
  withr::local_envvar(RSTUDIO_PRODUCT = "CONNECT")
  session <- structure(list(request = list()), class = "ShinySession")
  expect_snapshot(. <- chat_cortex(
    "testorg-test_account",
    model_file = "model.yaml",
    session = session
  ), error = TRUE)
})

test_that("token exchange requests to Connect look correct", {
  # Mock a Connect environment that supports viewer-based credentials.
  withr::local_envvar(
    SNOWFLAKE_ACCOUNT = "testorg-test_account",
    RSTUDIO_PRODUCT = "CONNECT",
    CONNECT_SERVER = "localhost:3030",
    CONNECT_API_KEY = "key"
  )
  local_mocked_responses(function(req) {
    # Snapshot relevant fields of the outgoing request.
    expect_snapshot(
      list(url = req$url, headers = req$headers, body = req$body$data)
    )
    response_json(body = list(access_token = "token"))
  })
  session <- structure(
    list(request = list(HTTP_POSIT_CONNECT_USER_SESSION_TOKEN = "user-token")),
    class = "ShinySession"
  )
  expect_equal(cortex_credentials(session = session), "token")
})
