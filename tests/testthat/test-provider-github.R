test_that("uses to GITHUB_PAT if set", {
  withr::local_envvar(
    GITHUB_PAT = "abc",
    GITHUB_PAT_GITHUB_COM = NA
  )
  expect_equal(github_key(), "abc")
})
