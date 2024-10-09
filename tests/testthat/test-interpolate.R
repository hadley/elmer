test_that("checks inputs", {
  expect_snapshot(error = TRUE, {
    interpolate(1)
    interpolate("x", 1)
    interpolate("{{x}}", x = 1:2)
  })
})

test_that("can interpolate from local env or from ...", {
  x <- 1

  expect_equal(interpolate("{{x}}"), glue::glue("1"))
  expect_equal(interpolate("{{x}}", x = 2), glue::glue("2"))
})

test_that("can interpolate from a file", {
  path <- withr::local_tempfile(lines = "{{x}}")
  expect_equal(interpolate_file(path, x = 1), glue::glue("1"))
})
