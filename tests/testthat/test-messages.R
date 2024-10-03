test_that("system prompt is applied correctly", {
  sys_prompt <- "foo"
  sys_msg <- chat_message("system", sys_prompt)
  user_msg <- chat_message("user", "bar")

  expect_equal(normalize_messages(), list())
  expect_equal(normalize_messages(list(user_msg)), list(user_msg))
  expect_equal(normalize_messages(list(sys_msg)), list(sys_msg))

  expect_equal(normalize_messages(list(), sys_prompt), list(sys_msg))
  expect_equal(
    normalize_messages(list(user_msg), sys_prompt),
    list(sys_msg, user_msg)
  )
  expect_equal(
    normalize_messages(list(sys_msg, user_msg), sys_prompt),
    list(sys_msg, user_msg)
  )
})

test_that("normalize_messages throws useful errors", {
  sys_prompt <- "foo"
  sys_msg <- chat_message("system", "foo")
  user_msg <- chat_message("user", "bar")

  expect_snapshot(error = TRUE, {
    normalize_messages(1)
    normalize_messages(list(1))
    normalize_messages(list(sys_msg, user_msg), 1)
    normalize_messages(list(sys_msg, user_msg), "foo2")
  })
})
