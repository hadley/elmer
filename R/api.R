# Currently performing chat request is not generic as there appears to
# be sufficiently genericity elsewhere to handle the API variations.
# We will recconsider this in the future if necessary.
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

chat_perform_value <- function(model, req) {
  resp_body_json(req_perform(req))
}

on_load(chat_perform_stream <- coro::generator(function(model, req) {
  resp <- httr2::req_perform_connection(req)
  on.exit(close(resp))
  reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- httr2::resp_stream_sse(resp)
    if (is.null(event)) {
      abort("Connection failed")
    } else if (stream_is_done(model, event)) {
      break
    } else {
      yield(stream_parse(model, event))
    }
  }

  # Work around https://github.com/r-lib/coro/issues/51
  if (FALSE) {
    yield(NULL)
  }
}))

chat_perform_async_value <- function(model, req) {
  promises::then(req_perform_promise(req), resp_body_json)
}

on_load(chat_perform_async_stream <- coro::async_generator(function(model, req, polling_interval_secs = 0.1) {
  resp <- req_perform_connection(req, blocking = FALSE)
  on.exit(close(resp))
  # TODO: Investigate if this works with async generators
  # reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      # TODO: Detect if connection is closed and stop polling
      await(coro::async_sleep(polling_interval_secs))
    } else if (stream_is_done(model, event)) {
      break
    } else {
      yield(stream_parse(model, event))
    }
  }

  # Work around https://github.com/r-lib/coro/issues/51
  if (FALSE) {
    yield(NULL)
  }
}))

# Create a request------------------------------------

chat_request <- new_generic("chat_request", "model",
  function(model, stream = TRUE, messages = list(), tools = list(), extra_args = list()) {
    S7_dispatch()
  })

# Extract data from streaming results ------------------------------------

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

# Extract data from non-streaming results --------------------------------------

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
