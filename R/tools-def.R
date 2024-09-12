
tool_def <- function(name, description, arguments, strict = FALSE) {
  arg_names <- names(arguments)
  arg_required <- map_lgl(arguments, function(arg) {
    arg$required %||% FALSE
  })

  properties <- lapply(arguments, function(x) x[c("type", "description")])
  names(properties) <- arg_names

  list(
    type = "function",
    "function" = list(
      name = name,
      description = description,
      strict = strict,
      parameters = list(
        type = "object",
        properties = properties,
        required = as.list(arg_names[arg_required]),
        additionalProperties = FALSE
      )
    )
  )
}

#' Define arguments for a tool
#'
#' @export
#' @param type Argument type.
#' @param description Description of argument as free text.
#' @param required Is the argument required?
#' @param ... Additional JSON Schema properties (e.g. `properties`, `enum`,
#'   `pattern`).
#' @keywords internal
tool_arg <- function(type, description, ..., required = TRUE) {
  check_string(type)
  check_string(description)
  check_bool(required)

  list(
    type = type,
    description = description,
    required = required,
    ...
  )
}
