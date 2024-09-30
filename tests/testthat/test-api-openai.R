test_that("system prompt is applied correctly", {
  sys_prompt <- "foo"
  sys_msg <- list(role = "system", content = sys_prompt)
  user_msg <- list(role = "user", content = "bar")

  expect_equal(
    openai_apply_system_prompt(NULL, list()),
    list()
  )
  expect_equal(
    openai_apply_system_prompt(NULL, list(user_msg)),
    list(user_msg)
  )
  expect_equal(
    openai_apply_system_prompt(sys_prompt, NULL),
    list(sys_msg)
  )
  expect_equal(
    openai_apply_system_prompt(sys_prompt, list()),
    list(sys_msg)
  )
  expect_equal(
    openai_apply_system_prompt(sys_prompt, list(user_msg)),
    list(sys_msg, user_msg)
  )
  expect_equal(
    openai_apply_system_prompt(sys_prompt, list(sys_msg, user_msg)),
    list(sys_msg, user_msg)
  )

  sys_msg2 <- list(role = "system", content = "baz")
  expect_error(
    openai_apply_system_prompt(sys_prompt, list(sys_msg2, user_msg))
  )

  chat <- new_chat_openai(system_prompt = sys_prompt, messages = list(user_msg))
  expect_identical(chat$system_prompt, sys_prompt)
  expect_identical(chat$messages(), list(user_msg))
  expect_identical(chat$messages(include_system_prompt = TRUE), list(sys_msg, user_msg))
})

test_that("default model is reported", {
  expect_snapshot(. <- new_chat_openai()$chat("Hi"))
})


# Tool calls -------------------------------------------------------------------

test_that("can make a simple tool call", {
  get_date <- function() "2024-01-01"
  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(get_date, "get_date", "Gets the current date", list())

  result <- chat$chat("What's the current date?")
  expect_equal(result, "2024-01-01")

  result <- chat$chat("What day of the week is it?")
  expect_equal(result, "Tuesday")
})

test_that("can make an async tool call", {
  get_date <- coro::async(function() {
    await(coro::async_sleep(0.2))
    "2024-01-01"
  })
  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(get_date, "get_date", "Gets the current date", list())

  result <- sync(chat$chat_async("What's the current date?"))
  expect_equal(result, "2024-01-01")

  expect_snapshot(chat$chat("Great. Do it again."), error = TRUE)
})

test_that("can call multiple tools in parallel", {
  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  favourite_color <- function(person) {
    if (person == "Joe") "sage green" else "red"
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
  expect_identical(result, "Joe: sage green, Hadley: red")
  expect_length(chat$messages(include_system_prompt = FALSE), 5)
})

test_that("can call multiple tools in sequence", {
  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(
    function() 2024,
    "get_year",
    "Get the current year",
    list()
  )
  chat$register_tool(
    function(year) if (year == 2024) "Susan" else "I don't know",
    "popular_name",
    "Gets the most popular name for a year",
    list(year = tool_arg("integer", "Year"))
  )

  result <- chat$chat("What was the most popular name this year?")
  expect_equal(result, "Susan")
  expect_length(chat$messages(include_system_prompt = FALSE), 6)
})
