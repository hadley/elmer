test_that("help topic extraction works", {
  print_help <- get_help_text("print")
  expect_type(print_help, "character")
  expect_gt(nchar(print_help), 50)

  expect_identical(get_help_text("print", "base"), print_help)
})

#' A function for foo-ing three numbers.
#'
#' @param x The first param
#' @param y The second param
#' @param z Take a guess
#' @returns The result of x %foo% y %foo% z.
has_roxygen_comments <- function(x, y, z = pi - 3.14) {
  super_secret_code()
}

  #' A function for foo-ing three numbers.
  #'
  #' @param x The first param
  #' @param y The second param
  #' @param z Take a guess
  #' @returns The result of x %foo% y %foo% z.
  indented_comments <- function(x, y, z = pi - 3.14) {
    super_secret_code()
  }

no_roxygen_comments <- function(i, j, k = pi - 3.14) {
  super_secret_code()
}

no_srcfile <- eval(quote(
  #' A function for foo-ing three numbers.
  function(a, b, c = pi - 3.14) {
    super_secret_code()
  }
))

test_that("roxygen2 comment extraction works", {
  aliased_function <- has_roxygen_comments

  expect_snapshot(extract_comments_and_signature(has_roxygen_comments))
  expect_snapshot(extract_comments_and_signature(aliased_function))
  expect_snapshot(extract_comments_and_signature(indented_comments))
  expect_snapshot(extract_comments_and_signature(no_srcfile))
})

test_that("basic signature extraction works", {
  expect_snapshot(extract_comments_and_signature(no_roxygen_comments))
})
