test_that("can make simple batch request", {
  chat <- chat_databricks(
    system_prompt = "Be as terse as possible; no punctuation"
  )
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  # Note: the Databricks models don't seem to take "as terse as possible" very
  # seriously, and return more verbose responses than expected.
  # expect_equal(chat$last_turn()@tokens, c(26, 5))
})

test_that("can make simple streaming request", {
  chat <- chat_databricks(
    system_prompt = "Be as terse as possible; no punctuation"
  )
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  # Setting a dummy host ensures we don't skip this test, even if there are no
  # Databricks credentials available.
  withr::local_envvar(DATABRICKS_HOST = "https://example.cloud.databricks.com")
  expect_snapshot(. <- chat_databricks())
})

test_that("respects turns interface", {
  # Note: Databricks models cannot handle the prompt for uppercase response,
  # so skip test_turn_system().
  # test_turns_system(chat_databricks)
  test_turns_existing(chat_databricks)
})

test_that("all tool variations work", {
  # Note: Databricks models cannot yet handle "continuing past the first tool
  # call", which causes issues with how ellmer implements tool calling. Nor do
  # they support parallel tool calls.
  #
  # See: https://docs.databricks.com/en/machine-learning/model-serving/function-calling.html#limitations

  # test_tools_simple(chat_databricks)
  # test_tools_async(chat_databricks)
  # test_tools_parallel(chat_databricks)
  # test_tools_sequential(chat_databricks, total_calls = 6)
})

test_that("can extract data", {
  test_data_extraction(chat_databricks)
})

test_that("can use images", {
  # Databricks models don't support images.
  #
  # test_images_inline(chat_databricks)
  # test_images_remote(chat_databricks)
})

# Auth --------------------------------------------------------------------

test_that("Databricks PATs are detected correctly", {
  withr::local_envvar(
    DATABRICKS_HOST = "https://example.cloud.databricks.com",
    DATABRICKS_TOKEN = "token"
  )
  expect_equal(databricks_token(), "token")
  expect_equal(databricks_token(token = "another token"), "another token")
})

test_that("M2M authentication requests look correct", {
  withr::local_envvar(
    DATABRICKS_HOST = "https://example.cloud.databricks.com",
    DATABRICKS_CLIENT_ID = "id",
    DATABRICKS_CLIENT_SECRET = "secret"
  )
  local_mocked_responses(function(req) {
    # Snapshot relevant fields of the outgoing request.
    expect_snapshot(
      list(url = req$url, headers = req$headers, body = req$body$data)
    )
    response_json(body = list(access_token = "token"))
  })
  expect_equal(databricks_token(), "token")
})
