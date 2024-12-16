test_that("uses to GITHUB_PAT if set", {
  withr::local_envvar(GITHUB_PAT = "abc")
  expect_equal(github_key(), "abc")
})
