test_that("informative error if no key", {
  withr::local_envvar(OPENAI_API_KEY = NULL, TESTTHAT = "false")
  expect_snapshot(openai_key(), error = TRUE)
})

test_that("system prompt is applied correctly", {
  sys_prompt <- "foo"
  sys_msg <- list(role = "system", content = sys_prompt)
  user_msg <- list(role = "user", content = "bar")

  expect_equal(
    apply_system_prompt_openai(NULL, list()),
    list()
  )
  expect_equal(
    apply_system_prompt_openai(NULL, list(user_msg)),
    list(user_msg)
  )
  expect_equal(
    apply_system_prompt_openai(sys_prompt, NULL),
    list(sys_msg)
  )
  expect_equal(
    apply_system_prompt_openai(sys_prompt, list()),
    list(sys_msg)
  )
  expect_equal(
    apply_system_prompt_openai(sys_prompt, list(user_msg)),
    list(sys_msg, user_msg)
  )
  expect_equal(
    apply_system_prompt_openai(sys_prompt, list(sys_msg, user_msg)),
    list(sys_msg, user_msg)
  )

  sys_msg2 <- list(role = "system", content = "baz")
  expect_error(
    apply_system_prompt_openai(sys_prompt, list(sys_msg2, user_msg))
  )

  chat <- new_chat_openai(system_prompt = sys_prompt, messages = list(user_msg))
  expect_identical(chat$system_prompt, sys_prompt)
  expect_identical(chat$messages(), list(user_msg))
  expect_identical(chat$messages(include_system_prompt = TRUE), list(sys_msg, user_msg))
})
