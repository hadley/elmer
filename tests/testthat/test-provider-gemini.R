# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_gemini("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(17, 2))
})

test_that("can make simple streaming request", {
  chat <- chat_gemini("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_gemini())
})

test_that("respects turns interface", {
  chat_fun <- chat_gemini

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- chat_gemini

  # Gemini tool calls seem fairly unreliable so we retry once
  retry_test(test_tools_simple(chat_fun))
  retry_test(test_tools_async(chat_fun))
  retry_test(test_tools_parallel(chat_fun))
  retry_test(test_tools_sequential(chat_fun, total_calls = 6))
})

test_that("can extract data", {
  chat_fun <- chat_gemini

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- chat_gemini

  test_images_inline(chat_fun)
  test_images_remote_error(chat_fun)
})

# chunk merging ----------------------------------------------------------

test_that("can merge text output", {
  # output from "tell me a joke" with text changed
  messages <- c(
    '{"candidates": [{"content": {"parts": [{"text": "a"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 5,"totalTokenCount": 5},"modelVersion": "gemini-1.5-flash"}',
    '{"candidates": [{"content": {"parts": [{"text": "b"}],"role": "model"}}],"usageMetadata": {"promptTokenCount": 5,"totalTokenCount": 5},"modelVersion": "gemini-1.5-flash"}',
    '{"candidates": [{"content": {"parts": [{"text": "c"}],"role": "model"},"finishReason": "STOP"}],"usageMetadata": {"promptTokenCount": 5,"candidatesTokenCount": 17,"totalTokenCount": 22},"modelVersion": "gemini-1.5-flash"}'
  )
  chunks <- lapply(messages, jsonlite::parse_json)

  out <- merge_gemini_chunks(chunks[[1]], chunks[[2]])
  out <- merge_gemini_chunks(out, chunks[[3]])

  expect_equal(out$candidates[[1]]$content$parts[[1]]$text, "abc")
  expect_equal(out$usageMetadata, list(
    promptTokenCount = 5,
    candidatesTokenCount = 17,
    totalTokenCount = 22
  ))
  expect_equal(out$candidates[[1]]$finishReason, "STOP")
})
