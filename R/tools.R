call_tools <- function(tools, message) {
  if (!has_name(message, "tool_calls")) {
    return()
  }

  lapply(message$tool_calls, function(call) {
    fun <- call$`function`
    tool_fun <- tools[[fun$name]]
    if (is.null(tool_fun)) {
      result <- paste0("Error calling tool: Unknown tool name '", call$`function`, "'")
    } else {
      args <- jsonlite::fromJSON(fun$arguments)
      result <- call_tool(tool_fun, args)
    }

    list(
      role = "tool",
      content = toString(result),
      tool_call_id = call$id
    )
  })
}

# Should we check `fun` against registered tools?
# Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases
call_tool <- function(fun, arguments) {
  tryCatch(
    do.call(fun, arguments),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      paste0("Error calling tool: ", conditionMessage(e))
    }
  )
}
