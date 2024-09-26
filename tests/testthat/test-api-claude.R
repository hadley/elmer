test_that("can perform a simple batch chat", {
  chat <- new_chat_claude(
    "You're a helpful assistant that returns very minimal output.
    If asked a math question, return only the answer.",
  )
  result <- chat$chat("What's 1 + 1")
  expect_equal(result[[1]]$text, "2")
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
  chat <- new_chat_claude(system_prompt = "Be very terse, not even punctuation.")
  get_date <- function() "2024-01-01"
  chat$register_tool(get_date, "get_date", "Gets the current date", list(), strict = TRUE)

  result <- chat$chat("What's the current date?")
  expect_match(result[[1]]$text, "2024-01-01")
})

test_that("can make an async tool call", {
  get_date <- coro::async(function() {
    await(coro::async_sleep(0.2))
    "2024-01-01"
  })
  chat <- new_chat_claude(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(get_date, "get_date", "Gets the current date", list())

  result <- sync(chat$chat_async("What's the current date?"))
  expect_match(result[[1]]$text, "2024-01-01")

  expect_snapshot(chat$chat("Great. Do it again."), error = TRUE)
})

test_that("can call multiple tools at once", {
  chat <- new_chat_claude(system_prompt = "Be very terse, not even punctuation.")
  favourite_color <- function(person) {
    if (person == "Joe") "blue" else "red"
  }
  chat$register_tool(
    favourite_color,
    "favourite_color",
    "Returns a person's favourite colour",
    list(person = tool_arg("string", "Name of a person")),
    strict = TRUE
  )

  result <- chat$chat("
    What are Joe and Hadley's favourite colours?
    Answer like name1: colour1, name2: colour2
  ")
  expect_identical(result[[1]]$text, "Joe: blue, Hadley: red")
})

test_that("can call multiple tools in sequence", {
  chat <- new_chat_claude(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(
    function() 2024,
    "tool1",
    "Get the current year",
    list()
  )
  chat$register_tool(
    function(year) if (year == 2024)"Susan" else "I don't know",
    "tool2",
    "Gets the most popular name for a year",
    list(year = tool_arg("integer", "Year"))
  )

  result <- chat$chat("
    What was the most popular name this year?
    Just provide the name.
  ")
  expect_equal(result[[1]]$text, "Susan")
})
