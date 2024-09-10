claude_chat <- function(messages,
                        tools = list(),
                        model = claude_model,
                        stream = TRUE,
                        base_url = claude_base_url,
                        api_key = claude_key(),
                        version = claude_version) {

  req <- claude_chat_request(
    messages = messages,
    tools = tools,
    base_url = base_url,
    model = model,
    stream = stream,
    api_key = api_key,
    version = version
  )

  if (stream) {
    chat_stream(
      req,
      is_done = function(event) identical(event$type, "message_stop"),
      parse_data = function(event) jsonlite::parse_json(event$data)
    )
  } else {
    resp <- httr2::req_perform(req)
    httr2::resp_body_json(resp)
  }
}


claude_chat_request <- function(messages,
                                stream = FALSE,
                                model = claude_model,
                                max_tokens = 1024,
                                metadata = NULL,
                                stop_sequences = NULL,
                                system = NULL,
                                temperature = NULL,
                                tool_choice = NULL,
                                tools = NULL,
                                top_k = NULL,
                                top_p = NULL,
                                base_url = claude_base_url,
                                api_key = claude_key(),
                                version = claude_version) {

  check_string(model)
  check_number_whole(max_tokens, min = 0)
  check_character(stop_sequences, allow_null = TRUE)
  check_string(system, allow_null = TRUE)
  check_number_whole(temperature, allow_null = TRUE)
  check_number_whole(top_k, allow_null = TRUE)
  check_number_whole(top_p, allow_null = TRUE)
  check_exclusive(temperature, top_p, .require = FALSE)

  body <- compact(list(
    model = model,
    max_tokens = max_tokens,
    messages = messages,
    metadata = metadata,
    stop_sequences = stop_sequences,
    stream = stream,
    system = system,
    temperature = temperature,
    tool_choice = tool_choice,
    tools = tools,
    top_k = top_k,
    top_p = top_p
  ))

  req <- claude_request(
    base_url = base_url,
    api_key = api_key,
    version = version
  )
  req <- httr2::req_url_path_append(req, "/messages")
  req <- httr2::req_body_json(req, body)
  req
}

claude_request <- function(base_url = claude_base_url,
                           api_key = claude_key(),
                           version = claude_version) {
  req <- httr2::request(base_url)
  req <- httr2::req_headers(req,
    # <https://docs.anthropic.com/en/api/versioning>
    `anthropic-version` = version,
    # <https://docs.anthropic.com/en/api/getting-started#authentication>
    `x-api-key` = api_key,
    .redact = "x-api-key"
  )

  # <https://docs.anthropic.com/en/api/rate-limits>
  req <- httr2::req_retry(req, max_tries = 2)

  # <https://docs.anthropic.com/en/api/errors>
  req <- httr2::req_error(req, body = function(resp) {
    json <- httr2::resp_body_json(resp)
    paste0(json$error$message, " [", json$error$type, "]")
  })
  req
}

claude_key <- function() get_key("ANTHROPIC_API_KEY")
claude_base_url <- "https://api.anthropic.com/v1/"
claude_model <- "claude-3-5-sonnet-20240620"
claude_version <- "2023-06-01"
