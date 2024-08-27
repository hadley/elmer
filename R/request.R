open_ai_request <- function(base_url = "https://api.openai.com/v1",
                            key = open_api_key()) {
  req <- httr2::request(base_url)
  req <- httr2::req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- httr2::req_retry(req, max_tries = 1)
  req <- httr2::req_error(req, body = function(resp) {
     httr2::resp_body_json(resp)$error$message
  })
  req
}

open_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(key, "")) {
    cli::cli_abort("Can't find env var {.code OPEN_API_KEY}.")
  }
  key
}
