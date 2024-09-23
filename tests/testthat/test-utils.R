test_that("finds key if set", {
  withr::local_envvar(FOO = "abc123")
  expect_true(key_exists("FOO"))
  expect_equal(key_get("FOO"), "abc123")
})


test_that("informative error if no key", {
  withr::local_envvar(FOO = NULL, TESTTHAT = "false")
  expect_false(key_exists("FOO"))
  expect_snapshot(key_get("FOO"), error = TRUE)
})
