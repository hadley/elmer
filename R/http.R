
chat_stream <- function(is_done, parse_data) {
  force(is_done)
  force(parse_data)

  coro::generator(function(req) {
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
}

openai_chat_stream <- coro::generator(function(req) {
  resp <- req_perform_connection(req)
  on.exit(close(resp))
  reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      abort("Connection failed")
    }
    if (event$data == "[DONE]") {
      break
    }
    json <- jsonlite::parse_json(event$data)
    yield(json)
  }

  # Work around https://github.com/r-lib/coro/issues/51
  if (FALSE) {
    yield(NULL)
  }
})
