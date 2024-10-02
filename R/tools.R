#' @include utils-S7.R
NULL

tool_call <- new_class(
  "tool_call",
  properties = list(
    id = prop_string(),
    name = prop_string(),
    arguments = class_list,
    parse_error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)

tool_result <- new_class(
  "tool_result",
  properties = list(
    id = prop_string(),
    result = class_any,
    error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
