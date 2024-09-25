test_that("can perform a simple batch chat", {
  chat <- new_chat_claude(
    "You're a helpful assistant that returns very minimal output.
    If asked a math question, return only the answer.",
  )
  result <- chat$chat("What's 1 + 1")
  expect_equal(result, "2")
  expect_equal(last_message(chat)$content[[1]]$text, "2")
})

test_that("can perform a simple streaming chat", {
  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n*$"
  chat <- new_chat_claude("
    You're a helpful assistant that returns very minimal output.
    When answering a question with multiple answers, put each answer on its own
    line with no punctuation."
  )

  chunks <- coro::collect(chat$stream("What are the canonical colors of the ROYGBIV rainbow?"))
  expect_gt(length(chunks), 2)
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(last_message(chat)$content[[1]]$text, rainbow_re, ignore.case = TRUE)
})

test_that("can call tools", {
  skip("not working")
  get_date <- function() "2024-01-01"
  chat <- new_chat_claude(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(get_date, "get_date", "Gets the current date", list(), strict = TRUE)

  result <- chat$chat("What's the current date?")
  expect_identical(paste(result, collapse = ""), "2024-01-01")
})
