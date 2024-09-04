
tool_def <- function(name, description, arguments, strict = TRUE) {
  arg_names <- map_chr(arguments, "[[", "name")
  arg_required <- map_lgl(arguments, "[[", "required")

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
        required = arg_names[arg_required],
        additionalProperties = FALSE
      )
    )
  )
}

#' Define arguments for a tool
#'
#' @export
#' @param name Name of the argument
#' @param type Argument type.
#' @param description Description of argument as free text.
#' @param required Is the argument required?
#' @keywords internal
tool_arg <- function(name, type, description, required = TRUE) {
  check_string(name)
  check_string(type)
  check_string(description)
  check_bool(required)

  list(
    name = name,
    type = type,
    description = description,
    required = required
  )
}
