gemini_chat <- function(messages,
                        tools = list(),
                        model = gemini_model,
                        stream = TRUE,
                        base_url = gemini_base_url,
                        api_key = gemini_key()) {
  req <- gemini_chat_request(
    messages = messages,
    tools = tools,
    base_url = base_url,
    model = model,
    stream = stream,
    api_key = api_key
  )

  if (stream) {
    chat_stream(
      req,
      is_done = function(event) is.null(event),
      parse_data = function(event) jsonlite::parse_json(event$data)
    )
  } else {
    resp <- httr2::req_perform(req)
    httr2::resp_body_json(resp)
  }
}

gemini_chat_request <- function(messages,
                                tools = list(),
                                model = gemini_model,
                                stream = TRUE,
                                base_url = gemini_base_url,
                                api_key = gemini_key()) {
  req <- gemini_request(base_url = base_url, api_key = api_key)
  if (stream) {
    req <- httr2::req_url_path_append(req, "models", paste0(model, ":", "streamGenerateContent"))
    req <- httr2::req_url_query(req, alt = "sse")
  } else {
    req <- httr2::req_url_path_append(req, "models", paste0(model, ":", "generateContent"))
  }
  req <- httr2::req_body_json(req, list(contents = messages))

  req
}

gemini_request <- function(base_url = gemini_base_url, api_key = gemini_key()) {
  req <- httr2::request(base_url)
  req <- httr2::req_headers(
    req,
    "x-goog-api-key" = api_key,
    .redact = "x-goog-api-key"
  )
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_error(req, body = function(resp) {
    browser()
  })
  req
}

gemini_model <- "gemini-1.5-flash"
gemini_base_url <- "https://generativelanguage.googleapis.com/v1beta/"
gemini_key <- function() get_key("GOOGLE_API_KEY")
