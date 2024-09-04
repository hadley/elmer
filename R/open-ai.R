open_ai_request <- function(base_url = "https://api.openai.com/v1",
                            key = open_ai_key()) {
  req <- httr2::request(base_url)
  req <- httr2::req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_error(req, body = function(resp) {
    httr2::resp_body_json(resp)$error$message
  })
  # req <- httr2::req_verbose(req, body_req = TRUE, body_resp = TRUE)
  req
}

open_ai_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(key, "")) {
    cli::cli_abort("Can't find env var {.code OPENAI_API_KEY}.")
  }
  key
}

open_ai_chat_req <- function(messages,
                             tools = list(),
                             base_url = "https://api.openai.com/v1",
                             model = "gpt-4o-mini",
                             stream = TRUE,
                             api_key = open_ai_key()) {
  if (length(tools) == 0) {
    # OpenAI rejects tools=[]
    tools <- NULL
  }

  data <- list(
    model = model,
    stream = stream,
    messages = messages,
    tools = tools
  )

  req <- open_ai_request(base_url = base_url, key = api_key)
  req <- httr2::req_url_path_append(req, "/chat/completions")
  req <- httr2::req_body_json(req, data)
  req
}

open_ai_chat <- function(messages,
                         tools = list(),
                         base_url = "https://api.openai.com/v1",
                         model = "gpt-4o-mini",
                         stream = TRUE,
                         api_key = open_ai_key()) {
  req <- open_ai_chat_req(
    messages = messages, tools = tools, base_url = base_url,
    model = model, stream = stream, api_key = api_key
  )

  if (stream) {
    open_ai_chat_stream(req)
  } else {
    resp <- httr2::req_perform(req)
    httr2::resp_body_json(resp)
  }
}

open_ai_chat_stream <- coro::generator(function(req) {
  resp <- httr2::req_perform_connection(req, mode = "text")
  on.exit(close(resp))
  reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- httr2::resp_stream_sse(resp)
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

open_ai_chat_async <- function(messages,
  tools = list(),
  base_url = "https://api.openai.com/v1",
  model = "gpt-4o-mini",
  stream = TRUE,
  api_key = open_ai_key()) {
  req <- open_ai_chat_req(
    messages = messages, tools = tools, base_url = base_url,
    model = model, stream = stream, api_key = api_key
  )

  if (stream) {
    open_ai_chat_stream_async(req)
  } else {
    resp <- httr2::req_perform_promise(req)
    promises::then(resp, httr2::resp_body_json)
  }
}

open_ai_chat_stream_async <- coro::async_generator(function(req, polling_interval_secs = 0.1) {
  resp <- httr2::req_perform_connection(req, mode = "text", blocking = FALSE)
  on.exit(close(resp))
  # TODO: Investigate if this works with async generators
  # reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- httr2::resp_stream_sse(resp)
    if (is.null(event)) {
      # TODO: Detect if connection is closed and stop polling
      await(coro::async_sleep(polling_interval_secs))
    } else if (event$data == "[DONE]") {
      break
    } else {
      json <- jsonlite::parse_json(event$data)
      yield(json)
    }
  }

  # Work around https://github.com/r-lib/coro/issues/51
  if (FALSE) {
    yield(NULL)
  }
})

