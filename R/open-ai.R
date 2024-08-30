open_ai_request <- function(base_url = "https://api.openai.com/v1",
                            key = open_ai_key()) {
  req <- httr2::request(base_url)
  req <- httr2::req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_error(req, body = function(resp) {
    httr2::resp_body_json(resp)$error$message
  })
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
})