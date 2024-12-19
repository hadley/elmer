#' Helpers for interpolating data into prompts
#'
#' These functions are lightweight wrappers around
#' [glue](https://glue.tidyverse.org/) that make it easier to interpolate
#' dynamic data into a static prompt. Compared to glue, these functions
#' expect you to wrap dynamic values in `{{ }}`, making it easier to include
#' R code and JSON in your prompt.
#'
#' @param prompt A prompt string. You should not generally expose this
#'   to the end user, since glue interpolation makes it easy to run arbitrary
#'   code.
#' @param ... Define additional temporary variables for substitution.
#' @param .envir Environment to evaluate `...` expressions in. Used when
#'   wrapping in another function. See `vignette("wrappers", package = "glue")`
#'   for more details.
#' @return A \{glue\} string.
#' @export
#' @examples
#' joke <- "You're a cool dude who loves to make jokes. Tell me a joke about {{topic}}."
#'
#' # You can supply valuese directly:
#' interpolate(joke, topic = "bananas")
#'
#' # Or allow interpolate to find them in the current environment:
#' topic <- "applies"
#' interpolate(joke)
interpolate <- function(prompt, ..., .envir = parent.frame()) {
  check_string(prompt)

  if (!dots_named(...)) {
    cli::cli_abort("All elements of `...` must be named")
  }

  out <- glue::glue(prompt, ..., .open = "{{", .close = "}}", .envir = .envir)

  if (length(out) != 1) {
    cli::cli_abort(c(
      "Must generate a single string.",
      i = "Did you accidentally include a vector in {.arg ...}`?"
    ))
  }

  out
}

#' @param path A path to a prompt file (often a `.md`).
#' @rdname interpolate
#' @export
interpolate_file <- function(path, ..., .envir = parent.frame()) {
  string <- read_file(path)
  interpolate(string, ..., .envir = .envir)
}

read_file <- function(path) {
  file_contents <- readChar(path, file.size(path))
}
