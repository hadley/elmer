test_that("can perform a simple batch chat", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output.
    If asked a math question, return only the answer.",
  )
  result <- chat$chat("What's 1 + 1")
  expect_equal(result, "2")
  expect_equal(last_text(chat), "2")

  result <- chat$chat_async("What's 1 + 1")
  expect_s3_class(result, "promise")
  result <- sync(result)
  expect_equal(result, "2")
  expect_equal(last_text(chat), "2")
})

test_that("can perform a simple streaming chat", {
  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  chat <- new_chat_openai("
    You're a helpful assistant that returns very minimal output.
    When answering a question with multiple answers, put each answer on its own
    line with no punctuation."
  )

  chunks <- coro::collect(chat$stream("What are the canonical colors of the ROYGBIV rainbow?"))
  expect_gt(length(chunks), 2)
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(last_text(chat), rainbow_re, ignore.case = TRUE)

  chunks <- coro::async_collect(chat$stream_async("What are the canonical colors of the ROYGBIV rainbow?"))
  expect_s3_class(chunks, "promise")
  chunks <- sync(chunks)
  expect_gt(length(chunks), 2)
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(last_text(chat), rainbow_re, ignore.case = TRUE)
})

test_that("has a basic print method", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output"
  )

  chat$chat("What's 1 + 1")
  expect_snapshot(chat)
})

test_that("can optionally echo", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output",
    echo = TRUE
  )
  expect_output(chat$chat("Echo this."), "Echo this.")
  expect_output(chat$chat("Echo this.", echo = FALSE), NA)

  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output"
  )
  expect_output(chat$chat("Echo this."), NA)
  expect_output(chat$chat("Echo this.", echo = TRUE), "Echo this.")
})
