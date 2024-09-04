test_that("informative error if no key", {
  withr::local_envvar(OPENAI_API_KEY = NULL, TESTTHAT = "false")
  expect_snapshot(openai_key(), error = TRUE)
})
