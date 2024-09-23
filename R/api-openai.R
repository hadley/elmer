openai_model <- function(base_url = "https://api.openai.com/v1",
                         model = "gpt-4o-mini",
                         seed = NULL,
                         extra_args = list(),
                         api_key = openai_key()) {
  structure(
    list(
      base_url = base_url,
      model = model,
      seed = seed,
      api_key = api_key,
      extra_args = list()
    ),
    class = "elmer::openai_model"
  )
}


openai_chat <- function(mode = c("value", "stream", "async-stream", "async-value"),
                        model,
                        messages,
                        tools = list()) {

  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")

  req <- openai_chat_req(
    messages = messages,
    tools = tools,
    model = model$model,
    seed = model$seed,
    stream = stream,
    base_url = model$base_url,
    api_key = model$api_key
  )

  handle_response <- switch(mode,
    "value" = function(req) resp_body_json(req_perform(req)),
    "async-value" = function(req) promises::then(req_perform_promise(req), resp_body_json),
    "stream" = openai_chat_stream,
    "async-stream" = openai_chat_stream_async
  )

  handle_response(req)
}

openai_chat_is_done <- function(event) {
  identical(event$data, "[DONE]")
}

openai_chat_parse <- function(event) {
  jsonlite::parse_json(event$data)
}

on_load(openai_chat_stream <- chat_stream(openai_chat_is_done, openai_chat_parse))
on_load(openai_chat_stream_async <- chat_stream_async(openai_chat_is_done, openai_chat_parse))

openai_chunk_text <- function(event, streaming) {
  if (streaming) {
    event$choices[[1]]$delta$content
  } else {
    event$choices[[1]]$message$content
  }
}
openai_merge_chunks <- function(cur, chunk) {
  if (is.null(cur)) {
    chunk
  } else {
    merge_dicts(cur, chunk)
  }
}
openai_result_message <- function(result, streaming) {
  if (streaming) {
    result$choices[[1]]$delta
  } else {
    result$choices[[1]]$message
  }
}

openai_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(key, "")) {
    cli::cli_abort("Can't find env var {.code OPENAI_API_KEY}.")
  }
  key
}

# https://platform.openai.com/docs/api-reference/chat/create
openai_chat_req <- function(messages,
                            tools = list(),
                            model = "gpt-4o-mini",
                            seed = NULL,
                            stream = TRUE,
                            extra_args = list(),
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key()) {

  check_string(model)
  check_number_whole(seed, allow_null = TRUE)
  check_bool(stream)

  data <- compact(list2(
    messages = messages,
    model = model,
    seed = seed,
    stream = stream,
    tools = tools,
    !!!extra_args
  ))

  req <- openai_request(base_url = base_url, key = api_key)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_body_json(req, data)
  req
}

openai_request <- function(base_url = "https://api.openai.com/v1",
                           key = openai_key()) {
  req <- request(base_url)
  req <- req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) {
     resp_body_json(resp)$error$message
  })
  # req <- req_verbose(req, body_req = TRUE, body_resp = TRUE)
  req
}

openai_key_exists <- function() {
  !identical(Sys.getenv("OPENAI_API_KEY"), "")
}

openai_key <- function() {
  if (openai_key_exists()) {
    Sys.getenv("OPENAI_API_KEY")
  } else {
    if (is_testing()) {
      testthat::skip("OPENAI_API_KEY env var is not configured")
    } else {
      cli::cli_abort("Can't find env var {.code OPENAI_API_KEY}.")
    }
  }
}
