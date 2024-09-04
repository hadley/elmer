test_that("can perform a simple chat with batch and streaming", {
  chat <- new_chat_openai(
    "You're a helpful assistant that returns very minimal output",
     quiet = TRUE
  )

  chat$chat("What's 1 + 1", stream = FALSE)
  expect_equal(last_message(chat)$content, "2")

  chat$chat("What's 2 + 2", stream = TRUE)
  expect_equal(last_message(chat)$content, "4")
})
