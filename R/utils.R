is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

is_snapshot <- function() {
  identical(Sys.getenv("TESTTHAT_IS_SNAPSHOT"), "true")
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

set_default <- function(value, default, arg = caller_arg(value)) {
  if (is.null(value)) {
    if (!is_testing() || is_snapshot()) {
      cli::cli_inform("Using {.field {arg}} = {.val {default}}.")
    }
    default
  } else {
    value
  }
}

last_request_json <- function() {
  print_json(last_request()$body$data)
}
last_response_json <- function() {
  print_json(resp_body_json(last_response()))
}
print_json <- function(x) {
  cat(pretty_json(x))
}
pretty_json <- function(x) {
  jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE)
}

check_echo <- function(echo = NULL) {
  if (is.null(echo) || identical(echo, c("none", "text", "all"))) {
    if (env_is_user_facing(parent.frame(2)) && !is_testing()) {
      "text"
    } else {
      "none"
    }
  } else if (isTRUE(echo)) {
    "text"
  } else if (isFALSE(echo)) {
    "none"
  } else {
    arg_match(echo, c("none", "text", "all"))
  }
}

dots_named <- function(...) {
  is_named2(list(...))
}

`paste<-` <- function(x, value) {
  paste0(x, value)
}

`append<-` <- function(x, value) {
  x[[length(x) + 1]] <- value
  x
}

is_azure_token <- function (object)
{
  R6::is.R6(object) && inherits(object, "AzureToken")
}



