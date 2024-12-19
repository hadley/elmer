#' @include utils-S7.R
#' @include types.R
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
#' @param .fun The function to be invoked when the tool is called.
#' @param .name The name of the function.
#' @param .description A detailed description of what the function does.
#'   Generally, the more information that you can provide here, the better.
#' @param ... Name-type pairs that define the arguments accepted by the
#'   function. Each element should be created by a [`type_*()`][type_boolean]
#'   function.
#' @export
#' @examplesIf ellmer:::openai_key_exists()
#'
#' # First define the metadata that the model uses to figure out when to
#' # call the tool
#' tool_rnorm <- tool(
#'   rnorm,
#'   "Drawn numbers from a random normal distribution",
#'   n = type_integer("The number of observations. Must be a positive integer."),
#'   mean = type_number("The mean value of the distribution."),
#'   sd = type_number("The standard deviation of the distribution. Must be a non-negative number.")
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
tool <- function(.fun, .description, ..., .name = NULL) {
  if (is.null(.name)) {
    fun_expr <- enexpr(.fun)
    if (is.name(fun_expr)) {
      .name <- as.character(fun_expr)
    } else {
      .name <- unique_tool_name()
    }
  }
  ToolDef(
    fun = .fun,
    name = .name,
    description = .description,
    arguments = type_object(...)
  )
}


ToolDef <- new_class(
  "ToolDef",
  properties = list(
    name = prop_string(),
    fun = class_function,
    description = prop_string(),
    arguments = TypeObject
  )
)
unique_tool_name <- function() {
  the$cur_tool_id <- (the$cur_tool_id %||% 0) + 1
  sprintf("tool_%03d", the$cur_tool_id)
}
