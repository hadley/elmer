#' @include utils-S7.R
NULL

content <- new_class("content", package = "elmer")

content_text <- new_class(
  "content_text",
  parent = content,
  properties = list(text = prop_string()),
  package = "elmer"
)

content_image <- new_class(
  "content_image",
  parent = content,
  package = "elmer"
)
content_image_remote <- new_class(
  "content_image_remote",
  parent = content_image,
  properties = list(
    url = prop_string(),
    detail = prop_string()
  ),
  package = "elmer"
)
content_image_inline <- new_class(
  "content_image_inline",
  parent = content_image,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)


method(format, content_text) <- function(x, ...) {
 # Using format_inline for word wrapping. Passing `"{x}"` instead of
 # `x` to avoid evaluation of the (potentially malicious) content.
  cli::format_inline("{x@text}")
}

method(format, content_image_inline) <- function(x, ...) {
  "[inline image]"
}

method(format, content_image_inline) <- function(x, ...) {
  cli::format_inline("[remote image]: {.url {x@url}}")
}

normalize_content <- function(provider, ..., error_call = caller_env()) {
  check_dots_unnamed(call = error_call)
  input <- rlang::list2(...)

  if (length(input) == 1 && is.character(input[[1]])) {
    list(from_provider(provider, input[[1]], error_call = error_call))
  } else {
    lapply(input, from_provider, provider = provider, error_call = error_call)
  }
}
