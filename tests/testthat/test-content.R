test_that("invalid inputs give useful errors", {
  chat <- new_chat_openai()

  expect_snapshot(error = TRUE, {
    chat$chat(question = "Are unicorns real?")
    chat$chat(TRUE)
  })
})
