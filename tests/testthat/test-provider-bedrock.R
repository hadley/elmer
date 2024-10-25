test_that("can make simple batch request", {
  chat <- chat_bedrock("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(26, 5))
})

test_that("can make simple streaming request", {
  chat <- chat_bedrock("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_bedrock())
})

test_that("respects turns interface", {
  chat_fun <- chat_bedrock

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- chat_bedrock

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- chat_bedrock

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_bedrock

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})
