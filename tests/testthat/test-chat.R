test_that("can perform a simple batch chat", {
  chat <- chat_openai()

  result <- chat$chat("What's 1 + 1. Just give me the answer, no punctuation")
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai()

  result <- chat$chat_async("What's 1 + 1. Just give me the answer, no punctuation")
  expect_s3_class(result, "promise")

  result <- sync(result)
  expect_equal(result, "2")
  expect_equal(chat$last_turn()@contents[[1]]@text, "2")
})

test_that("can perform a simple streaming chat", {
  chat <- chat_openai()

  chunks <- coro::collect(chat$stream("
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "))
  expect_gt(length(chunks), 2)

  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(chat$last_turn()@contents[[1]]@text, rainbow_re, ignore.case = TRUE)
})

test_that("can perform a simple async batch chat", {
  chat <- chat_openai()

  chunks <- coro::async_collect(chat$stream_async("
    What are the canonical colors of the ROYGBIV rainbow?
    Put each colour on its own line. Don't use punctuation.
  "))
  expect_s3_class(chunks, "promise")

  chunks <- sync(chunks)
  expect_gt(length(chunks), 2)
  rainbow_re <- "^red *\norange *\nyellow *\ngreen *\nblue *\nindigo *\nviolet *\n?$"
  expect_match(paste(chunks, collapse = ""), rainbow_re, ignore.case = TRUE)
  expect_match(chat$last_turn()@contents[[1]]@text, rainbow_re, ignore.case = TRUE)
})

test_that("has a basic print method", {
  chat <- chat_openai(
    "You're a helpful assistant that returns very minimal output",
    turns = list(
      Turn("user", "What's 1 + 1? What's 1 + 2?"),
      Turn("assistant", "2\n\n3", tokens = c(15, 5))
    )
  )
  expect_snapshot(chat)
})

test_that("can optionally echo", {
  chat <- chat_openai(
    "You're a helpful assistant that returns very minimal output",
    echo = TRUE
  )
  expect_output(chat$chat("Echo this."), "Echo this.")
  expect_output(chat$chat("Echo this.", echo = FALSE), NA)

  chat <- chat_openai(
    "You're a helpful assistant that returns very minimal output"
  )
  expect_output(chat$chat("Echo this."), NA)
  expect_output(chat$chat("Echo this.", echo = TRUE), "Echo this.")
})

test_that("can retrieve last_turn for user and assistant", {
  chat <- chat_openai()
  expect_equal(chat$last_turn("user"), NULL)
  expect_equal(chat$last_turn("assistant"), NULL)

  chat$chat("Hi")
  expect_equal(chat$last_turn("user")@role, "user")
  expect_equal(chat$last_turn("assistant")@role, "assistant")
})

test_that("can retrieve system prompt with helper or last_turn()", {
  chat1 <- chat_openai()
  expect_equal(chat1$system_prompt, NULL)
  expect_equal(chat1$last_turn("system"), NULL)

  chat2 <- chat_openai(system_prompt = "You are from New Zealand")
  expect_equal(chat2$system_prompt, "You are from New Zealand")
  expect_equal(chat2$last_turn("system"), Turn("system", "You are from New Zealand"))
})

test_that("can modify the system prompt", {
  chat <- chat_openai(turns = list(
    Turn("user", "Hi"),
    Turn("assistant", "Hello")
  ))

  # NULL -> NULL
  chat$system_prompt <- NULL
  expect_equal(chat$system_prompt, NULL)

  # NULL -> string
  chat$system_prompt <- "x"
  expect_equal(chat$system_prompt, "x")

  # string -> string
  chat$system_prompt <- "y"
  expect_equal(chat$system_prompt, "y")

  # string -> NULL
  chat$system_prompt <- NULL
  expect_equal(chat$system_prompt, NULL)
})
