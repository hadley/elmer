test_that("can perform a simple chat with batch and streaming", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output. If asked a math question, return only the answer.",
    quiet = TRUE
  )

  result <- chat$chat("What's 1 + 1")
  expect_equal(result, "2")
  expect_equal(last_message(chat)$content, "2")

  chunks <- coro::collect(chat$stream("What's 2 + 2"))
  expect_equal(paste(chunks, collapse = ""), "4\n")
  expect_equal(last_message(chat)$content, "4")

  chunks <- coro::collect(chat$stream("What are the canonical colors of the ROYGBIV rainbow? Answer with each color on its own line, no punctuation."))
  expect_gt(length(chunks), 2)
  expect_match(last_message(chat)$content, "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *$", ignore.case = TRUE)
})

test_that("has a basic print method", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output",
    quiet = TRUE
  )

  chat$chat("What's 1 + 1")
  expect_snapshot(chat)
})
