test_that("can perform a simple chat with batch and streaming", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output",
     quiet = TRUE
  )

  chat$chat("What's 1 + 1")
  expect_equal(last_message(chat)$content, "2")

  coro::collect(chat$stream("What's 2 + 2"))
  expect_equal(last_message(chat)$content, "4")
})

test_that("has a basic print method", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output",
     quiet = TRUE
  )

  chat$chat("What's 1 + 1")
  expect_snapshot(chat)
})
