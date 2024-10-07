#' @include api.R
#' @include content.R
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
#' @export
#' @returns A [Chat] object.
new_chat_claude <- function(system_prompt = NULL,
                            turns = NULL,
                            max_tokens = 4096,
                            model = NULL,
                            api_args = list(),
                            base_url = "https://api.anthropic.com/v1",
                            api_key = anthropic_key(),
                            echo = FALSE) {
  turns <- normalize_turns(turns, system_prompt)
  check_bool(echo)

  model <- model %||% "claude-3-5-sonnet-20240620"

  provider <- new_claude_provider(
    model = model,
    max_tokens = max_tokens,
    extra_args = api_args,
    base_url = base_url,
    api_key = api_key
  )

  Chat$new(provider = provider, turns = turns, echo = echo)
}

new_claude_provider <- function(model,
                                max_tokens = 4096,
                                extra_args = list(),
                                api_key = anthropic_key(),
                                base_url = "https://api.anthropic.com/v1",
                                error_call = caller_env()) {

  # These checks could/should be placed in the validator, but the S7 object is
  # currently an implementation detail. Keeping these errors here avoids
  # leaking that implementation detail to the user.

  check_string(model, call = error_call)
  check_number_whole(max_tokens, min = 1, call = error_call)
  # check_named_list(extra_args, call = error_call())
  check_string(base_url, call = error_call)
  check_string(api_key, call = error_call)

  claude_provider(
    model = model,
    max_tokens = max_tokens,
    extra_args = extra_args,
    base_url = base_url,
    api_key = api_key
  )
}

claude_provider <- new_class(
  "claude_provider",
  package = "elmer",
  properties = list(
    model = class_character,
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

method(chat_request, claude_provider) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  extra_args = list()) {

  req <- httr2::request(provider@base_url)
  # https://docs.anthropic.com/en/api/messages
  req <- httr2::req_url_path_append(req, "/messages")

  req <- httr2::req_headers(req,
    # <https://docs.anthropic.com/en/api/versioning>
    `anthropic-version` = "2023-06-01",
    # <https://docs.anthropic.com/en/api/getting-started#authentication>
    `x-api-key` = provider@api_key,
    .redact = "x-api-key"
  )

  # <https://docs.anthropic.com/en/api/rate-limits>
  req <- httr2::req_retry(req, max_tries = 2)

  # <https://docs.anthropic.com/en/api/errors>
  req <- httr2::req_error(req, body = function(resp) {
    json <- httr2::resp_body_json(resp)
    paste0(json$error$message, " [", json$error$type, "]")
  })

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system = NULL
  }

  messages <- claude_messages(turns)
  tools <- lapply(tools, claude_tool)

  extra_args <- utils::modifyList(provider@extra_args, extra_args)
  body <- compact(list2(
    model = provider@model,
    system = system,
    messages = messages,
    stream = stream,
    max_tokens = provider@max_tokens,
    tools = tools,
    !!!extra_args
  ))
  req <- httr2::req_body_json(req, body)

  req
}

method(stream_is_done, claude_provider) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  } else {
    identical(event$type, "message_stop")
  }
}
method(stream_parse, claude_provider) <- function(provider, event) {
  jsonlite::parse_json(event$data)
}
method(stream_text, claude_provider) <- function(provider, event) {
  if (event$type == "content_block_delta") {
    event$delta$text
  }
}
method(stream_merge_chunks, claude_provider) <- function(provider, result, chunk) {
  if (chunk$type == "ping") {
    # nothing to do
  } else if (chunk$type == "message_start") {
    result$role <- chunk$message$role
  } else if (chunk$type == "content_block_start") {
    result$content[[chunk$index + 1L]] <- chunk$content_block
  } else if (chunk$type == "content_block_delta") {
    if (!is.null(chunk$delta$text)) {
      result$content[[chunk$index + 1L]]$text <-
        paste0(result$content[[chunk$index + 1L]]$text, chunk$delta$text)
    }
  } else if (chunk$type == "content_block_stop") {
    # nothing to do
  } else if (chunk$type == "message_delta") {
    # TODO: do something with stop reason
  } else {
    cli::cli_inform(c("!" = "Unknown chunk type {.str {chunk$type}}."))
  }
  result
}
method(stream_turn, claude_provider) <- function(provider, result) {
  contents <- lapply(result$content, function(content) {
    if (content$type == "text") {
      content_text(content$text)
    } else if (content$type == "tool_use") {
      content_tool_request(content$id, content$name, content$input)
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  })

  turn(result$role, contents)
}
method(value_turn, claude_provider) <- method(stream_turn, claude_provider)


# Convert elmer turns + content to claude messages ----------------------------
claude_messages <- function(turns) {
  messages <- list()
  add_message <- function(role, ...) {
    messages[[length(messages) + 1]] <<- compact(list(role = role, ...))
  }

  for (turn in turns) {
    if (turn@role == "system") {
      # claude passes system prompt as separate arg
    } else if (turn@role %in% c("user", "assistant")) {
      content <- lapply(turn@contents, claude_content)
      add_message(turn@role, content = content)
    } else {
      cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
    }
  }
  messages
}


claude_content <- new_generic("claude_content", "content")

method(claude_content, content_text) <- function(content) {
  list(type = "text", text = content@text)
}

method(claude_content, content_image_remote) <- function(content) {
  cli::cli_abort("Claude doesn't support remote images")
}

method(claude_content, content_image_inline) <- function(content) {
  list(
    type = "image",
    source = list(
      type = "base64",
      media_type = content@type,
      data = content@data
    )
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(claude_content, content_tool_request) <- function(content) {
  list(
    type = "tool_use",
    id = content@id,
    name = content@name,
    input = content@arguments
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(claude_content, content_tool_result) <- function(content) {
  if (is.null(content@result)) {
    result <- paste0("Tool calling failed with error ", content@error)
    is_error <- TRUE
  } else {
    result <- toString(content@result)
    is_error <- FALSE
  }

  list(
    type = "tool_result",
    tool_use_id = content@id,
    content = result,
    is_error = is_error
  )
}

claude_tool <- function(tool) {
  list(
    name = tool@name,
    description = tool@description,
    input_schema = compact(json_schema_parameters(tool@arguments))
  )
}
