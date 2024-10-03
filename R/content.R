#' @include utils-S7.R
NULL

content <- new_class("content", package = "elmer")

content_text <- new_class(
  "content_text",
  parent = content,
  properties = list(text = prop_string()),
  package = "elmer"
)
method(format, content_text) <- function(x, ...) {
  # Using format_inline for word wrapping. Passing `"{x}"` instead of
  # `x` to avoid evaluation of the (potentially malicious) content.
  cli::format_inline("{x@text}")
}

# Images -----------------------------------------------------------------

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
method(format, content_image_remote) <- function(x, ...) {
  cli::format_inline("[{.strong remote image}]: {.url {x@url}}")
}

content_image_inline <- new_class(
  "content_image_inline",
  parent = content_image,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
method(format, content_image_inline) <- function(x, ...) {
  cli::format_inline("[{.strong inline image}]")
}

# Tools ------------------------------------------------------------------

content_tool_request <- new_class(
  "content_tool_request",
  parent = content,
  properties = list(
    id = prop_string(),
    name = prop_string(),
    arguments = class_list,
    parse_error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
method(format, content_tool_request) <- function(x, ...) {
  if (length(x@arguments) == 0) {
    call <- call2(x@name)
  } else {
    call <- call2(x@name, x@arguments)
  }
  cli::format_inline("[{.strong tool request} ({x@id})]: {format(call)}")
}

content_tool_result <- new_class(
  "content_tool_result",
  parent = content,
  properties = list(
    id = prop_string(),
    result = class_any,
    error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
method(format, content_tool_result) <- function(x, ...) {
  cli::format_inline("[{.strong tool result}  ({x@id})]: {x@result}")
}
