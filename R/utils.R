is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}
