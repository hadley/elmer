# Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases
call_tool <- function(fun, arguments) {
  if (is.null(fun)) {
    return(paste0("Error calling tool: Unknown tool name '", call$`function`, "'"))
  }

  tryCatch(
    do.call(fun, arguments),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      paste0("Error calling tool: ", conditionMessage(e))
    }
  )
}
rlang::on_load(call_tool_async <- coro::async(function(fun, arguments) {
  if (is.null(fun)) {
    return(paste0("Error calling tool: Unknown tool name '", call$`function`, "'"))
  }

  tryCatch(
    await(do.call(fun, arguments)),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      paste0("Error calling tool: ", conditionMessage(e))
    }
  )
}))
