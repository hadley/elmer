openai_chat <- function(mode = c("batch", "stream", "async-stream", "async-batch"),
                        messages,
                        tools = list(),
                        base_url = "https://api.openai.com/v1",
                        model = "gpt-4o-mini",
                        api_key = openai_key()) {

  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "stream-async")

  req <- openai_chat_req(
    messages = messages,
    tools = tools,
    base_url = base_url,
    model = model,
    stream = stream,
    api_key = api_key
  )

  handle_response <- switch(mode,
    "batch" = function(req) resp_body_json(req_perform(req)),
    "async-batch" = function(req) promises::then(req_perform_promise(req), resp_body_json),
    "stream" = chat_stream(openai_chat_is_done, openai_chat_parse),
    "async-stream" = chat_stream_async(openai_chat_is_done, openai_chat_parse)
  )

  handle_response(req)
}

openai_chat_is_done <- function(event) {
  identical(event$data, "[DONE]")
}

openai_chat_parse <- function(event) {
  jsonlite::parse_json(event$data)
}

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

openai_stream_messages <- coro::generator(function(response) {

  result <- list()
  any_content <- FALSE

  for (chunk in response) {
    result <- merge_dicts(result, chunk)

    if (!is.null(chunk$choices[[1]]$delta$content)) {
      yield(chunk$choices[[1]]$delta$content)
      any_content <- TRUE
    }
  }

  list(
    handle_event = function(chunk) {
    },
    done = function() {
      if (any_content) {
        emit("\n")
        yield("\n")
      }
    },
    message = function() {
      result$choices[[1]]$delta
    }
  )

})

openai_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(key, "")) {
    cli::cli_abort("Can't find env var {.code OPENAI_API_KEY}.")
  }
  key
}

openai_chat_req <- function(messages,
                            tools = list(),
                            base_url = "https://api.openai.com/v1",
                            model = "gpt-4o-mini",
                            stream = TRUE,
                            api_key = openai_key()) {
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
