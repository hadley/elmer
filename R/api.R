chat_request <- new_generic("chat_request", "model")

chat_perform <- function(model,
                         mode = c("value", "stream", "async-stream", "async-value"),
                         messages,
                         tools = list(),
                         extra_args = list()) {

  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")

  req <- chat_request(
    model = model,
    messages = messages,
    tools = tools,
    stream = stream,
    extra_args = extra_args
  )

  switch(mode,
    "value" = chat_perform_value(model, req),
    "stream" = chat_perform_stream(model, req),
    "async-value" = chat_perform_async_value(model, req),
    "async-stream" = chat_perform_async_stream(model, req)
  )
}

# Various ways of performing the chat request
chat_perform_value <- new_generic("chat_value", "model",
  function(model, req) {
    S7_dispatch()
  }
)
chat_perform_stream <- new_generic("chat_stream", "model",
  function(model, req) {
    S7_dispatch()
  }
)
chat_perform_async_value <- new_generic("chat_async_value", "model",
  function(model, req) {
    S7_dispatch()
  }
)
chat_perform_async_stream <- new_generic("chat_async_stream", "model",
  function(model, req) {
    S7_dispatch()
  }
)

stream_is_done <- new_generic("stream_is_done", "model",
  function(model, event) {
    S7_dispatch()
  }
)
stream_parse <- new_generic("stream_parse", "model",
  function(model, event) {
    S7_dispatch()
  }
)
stream_text <- new_generic("stream_text", "model",
  function(model, event) {
    S7_dispatch()
  }
)
stream_merge_chunks <- new_generic("stream_merge_chunks", "model",
  function(model, result, chunk) {
    S7_dispatch()
  }
)
stream_message <- new_generic("stream_message", "model",
  function(model, result) {
    S7_dispatch()
  }
)

value_text <- new_generic("value_text", "model",
  function(model, event) {
    S7_dispatch()
  }
)
value_message <- new_generic("value_message", "model",
  function(model, result) {
    S7_dispatch()
  }
)
