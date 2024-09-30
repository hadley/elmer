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


test_that("can make a simple tool call", {
  get_date <- function() "2024-01-01"
  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(get_date, description = "Gets the current date")

  result <- chat$chat("What's the current date?")
  expect_equal(result, "2024-01-01")

  result <- chat$chat("What day of the week is it?")
  expect_equal(result, "Tuesday")
})

test_that("repeated tool calls (sync)", {
  not_actually_random_number <- 1

  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(
    fun = function(tz) format(as.POSIXct(as.POSIXct("2020-08-01 18:00:00", tz="UTC"), tz=tz)),
    "get_time",
    "Gets the current time",
    list("tz" = tool_arg(type = "string", description = "Time zone", required = TRUE)),
    strict = TRUE
  )
  chat$register_tool(
    fun = function(n, mean, sd) { not_actually_random_number },
    name = "rnorm",
    description = "Drawn numbers from a random normal distribution",
    arguments = list(
      "n" = tool_arg(type = "integer", description = "The number of observations. Must be a positive integer."),
      "mean" = tool_arg(type = "number", description = "The mean value of the distribution. Defaults to 0.", required = FALSE),
      "sd" = tool_arg(type = "number", description = "The standard deviation of the distribution. Must be a non-negative number. Defaults to 1.", required = FALSE)
    ),
    strict = FALSE
  )

  result <- coro::collect(chat$stream("Pick a random number. If it's positive, tell me the current time in New York. If it's negative, tell me the current time in Seattle. Use ISO-8601, e.g. '2006-01-02T15:04:05'."))
  expect_identical(paste(result, collapse = ""), "2020-08-01T14:00:00\n")

  not_actually_random_number <- -1
  result <- coro::collect(chat$stream("Great. Do it again."))
  expect_identical(paste(result, collapse = ""), "2020-08-01T11:00:00\n")

  expect_snapshot(chat)
})

test_that("repeated tool calls (async)", {
  not_actually_random_number <- 1

  chat_async <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.")
  chat_async$register_tool(
    fun = function(tz) format(as.POSIXct(as.POSIXct("2020-08-01 18:00:00", tz="UTC"), tz=tz)),
    "get_time",
    "Gets the current time",
    list("tz" = tool_arg(type = "string", description = "Time zone", required = TRUE)),
    strict = TRUE
  )
  # An async tool
  chat_async$register_tool(
    fun = coro::async(function(n, mean, sd) {
      await(coro::async_sleep(0.2))
      not_actually_random_number
    }),
    name = "rnorm",
    description = "Drawn numbers from a random normal distribution",
    arguments = list(
      "n" = tool_arg(type = "integer", description = "The number of observations. Must be a positive integer."),
      "mean" = tool_arg(type = "number", description = "The mean value of the distribution. Defaults to 0.", required = FALSE),
      "sd" = tool_arg(type = "number", description = "The standard deviation of the distribution. Must be a non-negative number. Defaults to 1.", required = FALSE)
    ),
    strict = FALSE
  )

  result <- sync(coro::async_collect(chat_async$stream_async("Pick a random number. If it's positive, tell me the current time in New York. If it's negative, tell me the current time in Seattle. Use ISO-8601.")))
  expect_identical(paste(result, collapse = ""), "2020-08-01T14:00:00\n")

  not_actually_random_number <- -1
  result <- sync(coro::async_collect(chat_async$stream_async("Great. Do it again.")))
  expect_identical(paste(result, collapse = ""), "2020-08-01T11:00:00\n")

  expect_snapshot(chat_async)

  # Can't use async tools with sync methods
  expect_error(chat_async$chat("Great. Do it again."), "chat_async")
})
