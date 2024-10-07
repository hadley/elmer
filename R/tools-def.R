#' @include utils-S7.R
NULL

#' Define a tool
#'
#' @description
#' Define an R function for use by a chatbot. The function will always be
#' run in the current R instance.
#'
#' Learn more in `vignette("tool-calling")`.
#'
#' @export
#' @param type Argument type (`"null"`, `"boolean"`, `"object"`, `"array"`,
#'   `"number"`, or `"string"`).
#' @param description Description of argument as free text.
#' @param required Is the argument required?
#' @param ... Additional provider specific JSON Schema properties
#'   (e.g. `properties`, `enum`, `pattern`).
#'
#'   For example, OpenAI supports a `strict` parameter that when `TRUE`
#'   enables [Structured Output](https://platform.openai.com/docs/guides/structured-outputs)
#'   mode, which comes with a number of [additional
#'   requirements](https://platform.openai.com/docs/guides/structured-outputs/supported-schemas).
#'
#' @order 2
#' @export
#' @examplesIf elmer:::openai_key_exists()
#'
#' # First define the metadata that the model uses to figure out when to
#' # call the tool
#' tool_rnorm <- ToolDef(
#'   rnorm,
#'   description = "Drawn numbers from a random normal distribution",
#'   arguments = list(
#'     n = ToolArg(
#'       type = "integer",
#'       description = "The number of observations. Must be a positive integer."
#'     ),
#'     mean = ToolArg(
#'       type = "number",
#'       description = "The mean value of the distribution."
#'     ),
#'     sd = ToolArg(
#'       type = "number",
#'       description = "The standard deviation of the distribution. Must be a non-negative number."
#'     )
#'   )
#' )
#' chat <- chat_openai()
#' # Then register it
#' chat$register_tool(tool_rnorm)
#'
#' # Then ask a question that needs it.
#' chat$chat("
#'   Give me five numbers from a random normal distribution.
#' ")
#'
#' # Look at the chat history to see how tool calling works:
#' # Assistant sends a tool request which is evaluated locally and
#' # results are send back in a tool result.
#' chat
ToolArg <- new_class(
  "ToolArg",
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
#' @param fun The function to be invoked when the tool is called.
#' @param name The name of the function.
#' @param description A detailed description of what the function does.
#'   Generally, the more information that you can provide here, the better.
#' @param arguments A named list of arguments that the function accepts.
#'   Should be a named list of objects created by [ToolArg()].
#' @rdname ToolArg
#' @export
ToolDef <- new_class(
  "ToolDef",
  properties = list(
    name = prop_string(),
    fun = class_function,
    description = prop_string(),
    arguments = prop_list_of(ToolArg, names = "all"),
    extra = class_list
  ),
  constructor = function(fun, name, description, arguments = list(), ...) {
    if (missing(name)) {
      fun_expr <- enexpr(fun)
      if (is.name(fun_expr)) {
        name <- as.character(fun_expr)
      } else {
        cli::cli_abort("{.arg name} is required when `fun` is defined inline.")
      }
    }

    new_object(
      S7_object(),
      name = name,
      fun = fun,
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
