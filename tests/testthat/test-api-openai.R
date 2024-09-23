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
