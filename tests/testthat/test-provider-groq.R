
# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_groq())
})

test_that("respects turns interface", {
  chat_fun <- function(...) chat_groq(..., model = "Llama-3.3-70b-Versatile")

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- function(...) chat_groq(..., model = "Llama-3.3-70b-Versatile")

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})
