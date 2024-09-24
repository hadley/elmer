is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

key_get <- function(name, error_call = caller_env()) {
  val <- Sys.getenv(name)
  if (!identical(val, "")) {
    val
  } else {
    if (is_testing()) {
      testthat::skip(sprintf("%s env var is not configured", name))
    } else {
      cli::cli_abort("Can't find env var {.code {name}}.", call = error_call)
    }
  }
}

key_exists <- function(name) {
  !identical(Sys.getenv(name), "")
}

defer <- function(expr, env = caller_env(), after = FALSE) {
  thunk <- as.call(list(function() expr))
  do.call(on.exit, list(thunk, TRUE, after), envir = env)
}
