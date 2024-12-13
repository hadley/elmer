# Currently performing chat request is not generic as there appears to
# be sufficiently genericity elsewhere to handle the API variations.
# We will recconsider this in the future if necessary.
chat_perform <- function(provider,
                         mode = c("value", "stream", "async-stream", "async-value"),
                         turns,
                         tools = list(),
                         type = NULL,
                         extra_args = list()) {

  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")

  req <- chat_request(
    provider = provider,
    turns = turns,
    tools = tools,
    stream = stream,
    type = type,
    extra_args = extra_args
  )

  switch(mode,
    "value" = chat_perform_value(provider, req),
    "stream" = chat_perform_stream(provider, req),
    "async-value" = chat_perform_async_value(provider, req),
    "async-stream" = chat_perform_async_stream(provider, req)
  )
}

chat_perform_value <- function(provider, req) {
  resp_body_json(req_perform(req))
}

on_load(chat_perform_stream <- coro::generator(function(provider, req) {
  resp <- req_perform_connection(req)
  on.exit(close(resp))

  repeat {
    event <- chat_resp_stream(provider, resp)
    data <- stream_parse(provider, event)
    if (is.null(data)) {
      break
    } else {
      yield(data)
    }
  }

}))

chat_perform_async_value <- function(provider, req) {
  promises::then(req_perform_promise(req), resp_body_json)
}

on_load(chat_perform_async_stream <- coro::async_generator(function(provider, req) {
  resp <- req_perform_connection(req, blocking = FALSE)
  on.exit(close(resp))

  repeat {
    event <- chat_resp_stream(provider, resp)
    if (is.null(event) && isIncomplete(resp$body)) {
      fds <- curl::multi_fdset(resp$body)
      await(promises::promise(function(resolve, reject) {
        later::later_fd(resolve, fds$reads, fds$writes, fds$exceptions, fds$timeout)
      }))
      next
    }

    data <- stream_parse(provider, event)
    if (is.null(data)) {
      break
    } else {
      yield(data)
    }
  }
}))
