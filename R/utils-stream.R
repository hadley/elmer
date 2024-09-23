chat_streamer <- function(model) {
  # silence R CMD check note
  yield <- NULL

  coro::generator(function(model, req) {
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
  })
}

chat_streamer_async <- function(polling_interval_secs = 0.1) {
  # silence R CMD check note
  yield <- await <- NULL
  force(polling_interval_secs)

  coro::async_generator(function(model, req) {
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
  })
}
