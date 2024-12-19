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

# The following functions are just wrappers around httr2::resp_stream_* and
# httr2::req_perform*, but with optional logging.

resp_stream_sse <- function(resp, max_size = Inf) {
  event <- httr2::resp_stream_sse(resp, max_size = max_size)
  if (!is.null(event) && log_http_traffic()) {
    cat_with_prefix("< ", paste0(names(event), ": ", event))
    cat("\n")
  }
  event
}

resp_stream_aws <- function(resp, max_size = Inf) {
  event <- httr2::resp_stream_aws(resp, max_size = max_size)
  if (!is.null(event) && log_http_traffic()) {
    # Emit header
    cat_with_prefix("< ", paste0(names(event$headers), ": ", event$headers))
    cat("\n")

    # Emit body
    cat_with_prefix("< ", event$body)
    cat("\n")
  }
  event
}

req_perform <- function(req, ...) {
  log_req_body(req)
  resp <- httr2::req_perform(req, ...)
  log_resp_body(resp)
  resp
}

req_perform_promise <- function(req, ...) {
  log_req_body(req)
  promises::then(httr2::req_perform_promise(req, ...), function(resp) {
    log_resp_body(resp)
    resp
  })
}

req_perform_connection <- function(req, blocking = TRUE) {
  log_req_body(req)
  httr2::req_perform_connection(req, blocking = blocking)
}

log_req_body <- function(req) {
  if (log_http_traffic()) {
    body <- req$body$data
    if (!is.null(body)) {
      cat_with_prefix("> ", jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
      cat("\n")
    }
  }
  invisible()
}

log_resp_body <- function(resp) {
  if (log_http_traffic()) {
    body <- resp_body_json(resp)
    if (!is.null(body)) {
      cat_with_prefix("< ", jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
      cat("\n")
    }
  }
  invisible()
}

# Prefixes each line of `lines` with `prefix`. `lines` must be a character
# vector but can be any length, and can contain embedded newlines or not.
cat_with_prefix <- function(prefix, lines) {
  if (length(lines) > 0) {
    lines <- unlist(strsplit(lines, "\n"))
    cat(paste0(prefix, lines, "\n"), sep = "")
  }
}

log_http_traffic <- function() {
  getOption("ellmer_verbosity", 0L) >= 2L
}
