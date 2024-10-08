test_that("checks inputs", {
  expect_snapshot(error = TRUE, {
    prompt(1)
    prompt("x", 1)
    prompt("{{x}}", x = 1:2)
  })
})

test_that("can interpolate from local env or from ...", {
  x <- 1

  expect_equal(prompt("{{x}}"), glue::glue("1"))
  expect_equal(prompt("{{x}}", x = 2), glue::glue("2"))
})

test_that("can interpolate from a file", {
  path <- withr::local_tempfile(lines = "{{x}}")
  expect_equal(prompt_file(path, x = 1), glue::glue("1"))
})
