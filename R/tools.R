call_tools <- function(message) {
  if (!has_name(message, "tool_calls")) {
    return()
  }

  lapply(message$tool_calls, function(call) {
    fun <- call$`function`
    args <- jsonlite::fromJSON(fun$arguments)
    result <- call_tool(fun$name, args)

    list(
      role = "tool",
      content = toString(result),
      tool_call_id = call$id
    )
  })
}

call_tool <- function(fun, arguments) {
  # How to handle errors?
  # Should we check `fun` against registered tools?
  # Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases

  do.call(fun, arguments)
}
