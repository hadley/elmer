#' @include api.R
NULL

#' Create a chatbot that speaks to Anthropic's Claude
#'
#' This function returns a [Chat] object that takes care of managing the state
#' associated with the chat; i.e. it records the messages that you send to the
#' server, and the messages that you receive back. If you register a tool
#' (aka an R function), it also takes care of the tool loop.
#'
#' @inheritParams new_chat_openai
#' @param max_tokens Maximum number of tokens to generate before stopping.
#' @param version Optional version specification if you want to opt-in to a
#'   beta version of a model.
#' @export
#' @returns A [Chat] object.
new_chat_claude <- function(system_prompt = NULL,
                            messages = NULL,
                            max_tokens = 4096,
                            version = NULL,
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            base_url = "https://api.anthropic.com/v1",
                            api_key = anthropic_key(),
                            echo = FALSE) {
  check_string(system_prompt, allow_null = TRUE)
  # claude_check_conversation(messages, allow_null = TRUE)
  check_bool(echo)

  model <- model %||% "claude-3-5-sonnet-20240620"
  version <- version %||% "2023-06-01"

  model <- new_claude_model(
    model = model,
    version = version,
    system = system_prompt,
    max_tokens = max_tokens,
    seed = seed,
    extra_args = api_args,
    base_url = base_url,
    api_key = api_key
  )

  Chat$new(model = model, messages = messages, echo = echo)
}

new_claude_model <- function(model,
                             version,
                             system = NULL,
                             seed = NULL,
                             max_tokens = 4096,
                             extra_args = list(),
                             api_key = anthropic_key(),
                             base_url = "https://api.anthropic.com/v1",
                             error_call = caller_env()) {

  # These checks could/should be placed in the validator, but the S7 object is
  # currently an implementation detail. Keeping these errors here avoids
  # leaking that implementation detail to the user.

  check_string(model, call = error_call())
  check_string(version, call = error_call())
  check_string(system, allow_null = TRUE, call = error_call())
  check_number_whole(seed, allow_null = TRUE, call = error_call())
  check_number_whole(max_tokens, min = 1, call = error_call())
  # check_named_list(extra_args, call = error_call())
  check_string(base_url, call = error_call())
  check_string(api_key, call = error_call())

  if (is_testing() && is.null(seed)) {
    seed <- 1014
  }

  claude_model(
    model = model,
    version = version,
    system = system,
    seed = seed,
    max_tokens = max_tokens,
    extra_args = extra_args,
    base_url = base_url,
    api_key = api_key
  )
}

claude_model <- new_class(
  "claude_model",
  package = "elmer",
  properties = list(
    model = class_character,
    version = class_character,
    system = class_character | NULL,
    seed = class_numeric | NULL,
    max_tokens = class_numeric,
    extra_args = class_list,
    base_url = class_character,
    api_key = class_character
  )
)

anthropic_key <- function() {
  key_get("ANTHROPIC_API_KEY")
}

# HTTP request and response handling -------------------------------------

method(chat_request, claude_model) <- function(model,
                                               stream = TRUE,
                                               messages = list(),
                                               tools = list(),
                                               extra_args = list()) {

  req <- httr2::request(model@base_url)
  # https://docs.anthropic.com/en/api/messages
  req <- httr2::req_url_path_append(req, "/messages")

  req <- httr2::req_headers(req,
    # <https://docs.anthropic.com/en/api/versioning>
    `anthropic-version` = model@version,
    # <https://docs.anthropic.com/en/api/getting-started#authentication>
    `x-api-key` = model@api_key,
    .redact = "x-api-key"
  )

  # <https://docs.anthropic.com/en/api/rate-limits>
  req <- httr2::req_retry(req, max_tries = 2)

  # <https://docs.anthropic.com/en/api/errors>
  req <- httr2::req_error(req, body = function(resp) {
    json <- httr2::resp_body_json(resp)
    paste0(json$error$message, " [", json$error$type, "]")
  })

  extra_args <- utils::modifyList(model@extra_args, extra_args)
  body <- compact(list2(
    model = model@model,
    messages = messages,
    seed = model@seed,
    stream = stream,
    system = model@system,
    max_tokens = model@max_tokens,
    tools = tools,
    !!!extra_args
  ))
  req <- httr2::req_body_json(req, body)

  req
}

method(stream_is_done, claude_model) <- function(model, event) {
  identical(event$type, "message_stop")
}
method(stream_parse, claude_model) <- function(model, event) {
  jsonlite::parse_json(event$data)
}
method(stream_text, claude_model) <- function(model, event) {
  if (event$type == "content_block_delta") {
    event$delta$text
  } else if (event$type == "content_block_stop") {
    "\n"
  }
}
method(stream_merge_chunks, claude_model) <- function(model, result, chunk) {
  if (chunk$type == "ping") {
    # next
  } else if (chunk$type == "message_start") {
    result$role <- chunk$message$role
  } else if (chunk$type == "content_block_start") {
    result$content[[chunk$index + 1L]] <- chunk$content_block
  } else if (chunk$type == "content_block_delta") {
    result$content[[chunk$index + 1L]]$text <-
      paste0(result$content[[chunk$index + 1L]]$text, chunk$delta$text)
  } else if (chunk$type == "content_block_stop") {
    #
  } else if (chunk$type == "message_delta") {
    # TODO: do something with stop reason
  } else {
    browser()
  }
  result
}
method(stream_message, claude_model) <- function(model, result) {
  result
}

method(value_text, claude_model) <- function(model, event) {
  event$content[[1]]$text
}
method(value_message, claude_model) <- function(model, result) {
  list(
    role = result$role,
    content = result$content
  )
}
