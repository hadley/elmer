test_that("informative error if no key", {
  withr::local_envvar(OPENAI_API_KEY = NULL)
  expect_snapshot(open_ai_key(), error = TRUE)
})
