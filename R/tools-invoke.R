invoke_tools <- function(tool_calls, tools) {
  lapply(tool_calls, function(call) {
    fun <- tools[[call@name]]
    result <- invoke_tool(fun, call@arguments, call@id)

    if (promises::is.promise(result@result)) {
      cli::cli_abort(c(
        "Can't use async tools with `$chat()` or `$stream()`.",
        i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
      ))
    }

    result
  })
}

rlang::on_load(
  invoke_tools_async <- coro::async(function(tool_calls, tools) {
    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_calls, function(call) {
      fun <- tools[[call@name]]
      invoke_tool_async(fun, call@arguments, call@id)
    })

    promises::promise_all(.list = result_promises)
  })
)

# Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(fun, arguments, id) {
  if (is.null(fun)) {
    return(tool_result(
      id = id,
      error = paste0("Unknown tool name '", call$`function`, "'")
    ))
  }

  tryCatch(
    tool_result(id, do.call(fun, arguments)),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      tool_result(id, error = conditionMessage(e))
    }
  )
}

rlang::on_load(invoke_tool_async <- coro::async(function(fun, arguments, id) {
  if (is.null(fun)) {
    return(tool_result(
      id = id,
      error = paste0("Unknown tool name '", call$`function`, "'")
    ))
  }

  tryCatch(
    {
      result <- await(do.call(fun, arguments))
      tool_result(id, result)
    },
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      tool_result(id, error = conditionMessage(e))
    }
  )
}))
