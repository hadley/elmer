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
  cli::format_inline("[{.strong inline image}]")
}

method(format, content_image_remote) <- function(x, ...) {
  cli::format_inline("[{.strong remote image}]: {.url {x@url}}")
}
