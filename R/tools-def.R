#' @include utils-S7.R
NULL

#' Define a tool
#'
#' @export
#' @param type Argument type (`"null"`, `"boolean"`, `"object"`, `"array"`,
#'   `"number"`, or `"string"`).
#' @param description Description of argument as free text.
#' @param required Is the argument required?
#' @param ... Additional JSON Schema properties (e.g. `properties`, `enum`,
#'   `pattern`).
#' @keywords internal
#' @order 2
#' @export
#' @examples
tool_arg <- new_class(
  "tool_arg",
  properties = list(
    type = prop_string(),
    description = prop_string(),
    required = prop_bool(),
    extra = class_list
  ),
  constructor = function(type, description, required = TRUE, ...) {
    new_object(
      S7_object(),
      type = type,
      description = description,
      required = required,
      extra = list(...)
    )
  },
  package = "elmer"
)

#' @order 1
#' @rdname tool_arg
#' @export
tool_def <- new_class(
  "tool_def",
  properties = list(
    name = prop_string(),
    description = prop_string(),
    arguments = prop_list_of(tool_arg, names = "all"),
    extra = class_list
  ),
  constructor = function(name, description, arguments = list(), ...) {
    new_object(
      S7_object(),
      name = name,
      description = description,
      arguments = arguments,
      extra = list(...)
    )
  },
  package = "elmer"
)

json_schema_parameters <- function(arguments) {
  arg_names <- names2(arguments)
  arg_required <- map_lgl(arguments, function(arg) {
    arg@required %||% FALSE
  })

  properties <- lapply(arguments, function(x) {
    list(type = x@type, description = x@description)
  })
  names(properties) <- arg_names

  list(
    type = "object",
    properties = properties,
    required = as.list(arg_names[arg_required]),
    additionalProperties = FALSE
  )
}
