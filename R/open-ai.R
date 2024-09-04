openai_chat <- function(messages,
                        tools = list(),
                        base_url = "https://api.openai.com/v1",
                        model = "gpt-4o-mini",
                        stream = TRUE,
                        api_key = openai_key(),
                        quiet = TRUE) {
  data <- list(
    model = model,
    stream = stream,
    messages = messages,
    tools = tools
  )

  req <- openai_request(base_url = base_url, key = api_key)
  req <- httr2::req_url_path_append(req, "/chat/completions")
  req <- httr2::req_body_json(req, data)

  if (stream) {
    resp <- httr2::req_perform_connection(req, mode = "text")
    on.exit(close(resp))

    results <- list()
    repeat({
      event <- httr2::resp_stream_sse(resp)
      if (is.null(event) || event$data == "[DONE]") {
        break
      }
      json <- jsonlite::parse_json(event$data)
      if (!quiet) {
        cat(json$choices[[1]]$delta$content)
      }
      results <- merge_dicts(results, json)
    })
  } else {
    resp <- httr2::req_perform(req)
    results <- httr2::resp_body_json(resp)

    if (!quiet) {
      cat(results$choices[[1]]$message$content)
    }
  }

  results
}

openai_request <- function(base_url = "https://api.openai.com/v1",
                           key = openai_key()) {
  req <- httr2::request(base_url)
  req <- httr2::req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- httr2::req_retry(req, max_tries = 2)
  req <- httr2::req_error(req, body = function(resp) {
     httr2::resp_body_json(resp)$error$message
  })
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
