#' @include utils-S7.R
#' @include content.R
NULL

content_tool_call <- new_class(
  "content_tool_call",
  parent = content,
  properties = list(
    id = prop_string(),
    name = prop_string(),
    arguments = class_list,
    parse_error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)

method(format, content_tool_call) <- function(x, ...) {
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
