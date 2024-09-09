test_that("call tool gives useful error", {
  expect_snapshot(call_tool("foo", c(1, 2, 3)))
})

test_that("repeated tool calls (sync)", {
  not_actually_random_number <- 1

  chat <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.", quiet = TRUE)
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

  chat_async <- new_chat_openai(system_prompt = "Be very terse, not even punctuation.", quiet = TRUE)
  chat_async$register_tool(
    fun = function(tz) format(as.POSIXct(as.POSIXct("2020-08-01 18:00:00", tz="UTC"), tz=tz)),
    "get_time",
    "Gets the current time",
    list("tz" = tool_arg(type = "string", description = "Time zone", required = TRUE)),
    strict = TRUE
  )
  chat_async$register_tool(
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

  result <- sync(coro::async_collect(chat_async$stream_async("Pick a random number. If it's positive, tell me the current time in New York. If it's negative, tell me the current time in Seattle. Use ISO-8601.")))
  expect_identical(paste(result, collapse = ""), "2020-08-01T14:00:00\n")

  not_actually_random_number <- -1
  result <- sync(coro::async_collect(chat_async$stream_async("Great. Do it again.")))
  expect_identical(paste(result, collapse = ""), "2020-08-01T11:00:00\n")

  expect_snapshot(chat_async)
})
