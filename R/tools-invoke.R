# Results a content list
invoke_tools <- function(message, tools) {
  tool_calls <- extract_tool_calls(message@content)

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

on_load(
  invoke_tools_async <- coro::async(function(message, tools) {
    tool_calls <- extract_tool_calls(message@content)

    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_calls, function(call) {
      fun <- tools[[call@name]]
      invoke_tool_async(fun, call@arguments, call@id)
    })

    promises::promise_all(.list = result_promises)
  })
)

extract_tool_calls <- function(contents) {
  is_tool_call <- map_lgl(contents, inherits, content_tool_call)
  contents[is_tool_call]
}

# Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(fun, arguments, id) {
  if (is.null(fun)) {
    return(content_tool_result(id = id, error = "Unknown tool"))
  }

  tryCatch(
    content_tool_result(id, do.call(fun, arguments)),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      content_tool_result(id, error = conditionMessage(e))
    }
  )
}

on_load(invoke_tool_async <- coro::async(function(fun, arguments, id) {
  if (is.null(fun)) {
    return(content_tool_result(id = id, error = "Unknown tool"))
  }

  tryCatch(
    {
      result <- await(do.call(fun, arguments))
      content_tool_result(id, result)
    },
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      content_tool_result(id, error = conditionMessage(e))
    }
  )
}))
