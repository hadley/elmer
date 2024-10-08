# Results a content list
invoke_tools <- function(turn, tools) {
  if (length(tools) == 0) {
    return()
  }
  tool_requests <- extract_tool_requests(turn@contents)

  lapply(tool_requests, function(call) {
    fun <- tools[[call@name]]@fun
    result <- invoke_tool(fun, call@arguments, call@id)

    if (promises::is.promise(result@value)) {
      cli::cli_abort(c(
        "Can't use async tools with `$chat()` or `$stream()`.",
        i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
      ))
    }

    result
  })
}

on_load(
  invoke_tools_async <- coro::async(function(turn, tools) {
    if (length(tools) == 0) {
      return()
    }

    tool_requests <- extract_tool_requests(turn@contents)

    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_requests, function(call) {
      fun <- tools[[call@name]]@fun
      invoke_tool_async(fun, call@arguments, call@id)
    })

    promises::promise_all(.list = result_promises)
  })
)

extract_tool_requests <- function(contents) {
  is_tool_request <- map_lgl(contents, S7_inherits, ContentToolRequest)
  contents[is_tool_request]
}

# Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases
invoke_tool <- function(fun, arguments, id) {
  if (is.null(fun)) {
    return(ContentToolResult(id = id, error = "Unknown tool"))
  }

  tryCatch(
    ContentToolResult(id, do.call(fun, arguments)),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      ContentToolResult(id, error = conditionMessage(e))
    }
  )
}

on_load(invoke_tool_async <- coro::async(function(fun, arguments, id) {
  if (is.null(fun)) {
    return(ContentToolResult(id = id, error = "Unknown tool"))
  }

  tryCatch(
    {
      result <- await(do.call(fun, arguments))
      ContentToolResult(id, result)
    },
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      ContentToolResult(id, error = conditionMessage(e))
    }
  )
}))
