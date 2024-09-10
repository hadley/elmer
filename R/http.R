
chat_stream <- function(req, is_done, parse_data) {
  force(is_done)
  force(parse_data)

  generator <- coro::generator(function(req) {
    resp <- httr2::req_perform_connection(req)
    on.exit(close(resp))
    reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

    while (TRUE) {
      event <- httr2::resp_stream_sse(resp)
      if (is_done(event)) {
        break
      }
      yield(parse_data(event))
    }

    # Work around https://github.com/r-lib/coro/issues/51
    if (FALSE) {
      yield(NULL)
    }
  })
  generator(req)
}
