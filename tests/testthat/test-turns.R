test_that("system prompt is applied correctly", {
  sys_prompt <- "foo"
  sys_msg <- turn("system", sys_prompt)
  user_msg <- turn("user", "bar")

  expect_equal(normalize_turns(), list())
  expect_equal(normalize_turns(list(user_msg)), list(user_msg))
  expect_equal(normalize_turns(list(sys_msg)), list(sys_msg))

  expect_equal(normalize_turns(list(), sys_prompt), list(sys_msg))
  expect_equal(
    normalize_turns(list(user_msg), sys_prompt),
    list(sys_msg, user_msg)
  )
  expect_equal(
    normalize_turns(list(sys_msg, user_msg), sys_prompt),
    list(sys_msg, user_msg)
  )
})

test_that("normalize_turns throws useful errors", {
  sys_prompt <- "foo"
  sys_msg <- turn("system", "foo")
  user_msg <- turn("user", "bar")

  expect_snapshot(error = TRUE, {
    normalize_turns(1)
    normalize_turns(list(1))
    normalize_turns(list(sys_msg, user_msg), 1)
    normalize_turns(list(sys_msg, user_msg), "foo2")
  })
})
