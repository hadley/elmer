#' @include provider.R
#' @include content.R
NULL

#' Create a chatbot that speaks to Anthropic's Claude
#'
#' This function returns a [Chat] object that takes care of managing the state
#' associated with the chat; i.e. it records the messages that you send to the
#' server, and the messages that you receive back. If you register a tool
#' (aka an R function), it also takes care of the tool loop.
#'
#' @inheritParams chat_openai
#' @param max_tokens Maximum number of tokens to generate before stopping.
#' @family chatbots
#' @export
#' @returns A [Chat] object.
chat_claude <- function(system_prompt = NULL,
                            turns = NULL,
                            max_tokens = 4096,
                            model = NULL,
                            api_args = list(),
                            base_url = "https://api.anthropic.com/v1",
                            api_key = anthropic_key(),
                            echo = NULL) {
  turns <- normalize_turns(turns, system_prompt)
  echo <- check_echo(echo)

  model <- model %||% "claude-3-5-sonnet-20240620"

  provider <- ProviderClaude(
    model = model,
    max_tokens = max_tokens,
    extra_args = api_args,
    base_url = base_url,
    api_key = api_key
  )

  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderClaude <- new_class(
  "ProviderClaude",
  parent = Provider,
  package = "elmer",
  properties = list(
    api_key = prop_string(),
    model = prop_string(),
    max_tokens = prop_number_whole(min = 1)
  )
)

anthropic_key <- function() {
  key_get("ANTHROPIC_API_KEY")
}

# HTTP request and response handling -------------------------------------

method(chat_request, ProviderClaude) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  extra_args = list()) {

  req <- request(provider@base_url)
  # https://docs.anthropic.com/en/api/messages
  req <- req_url_path_append(req, "/messages")

  req <- req_headers(req,
    # <https://docs.anthropic.com/en/api/versioning>
    `anthropic-version` = "2023-06-01",
    # <https://docs.anthropic.com/en/api/getting-started#authentication>
    `x-api-key` = provider@api_key,
    .redact = "x-api-key"
  )

  # <https://docs.anthropic.com/en/api/rate-limits>
  req <- req_retry(req, max_tries = 2)

  # <https://docs.anthropic.com/en/api/errors>
  req <- req_error(req, body = function(resp) {
    json <- resp_body_json(resp)
    paste0(json$error$message, " [", json$error$type, "]")
  })

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system = NULL
  }

  messages <- claude_messages(turns)
  tools <- unname(lapply(tools, claude_tool))

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
  req <- req_body_json(req, body)

  req
}

method(stream_parse, ProviderClaude) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  }

  data <- jsonlite::parse_json(event$data)
  if (identical(data$type, "message_stop")) {
    return(NULL)
  }

  data
}
method(stream_text, ProviderClaude) <- function(provider, event) {
  if (event$type == "content_block_delta") {
    event$delta$text
  }
}
method(stream_merge_chunks, ProviderClaude) <- function(provider, result, chunk) {
  if (chunk$type == "ping") {
    # nothing to do
  } else if (chunk$type == "message_start") {
    result <- chunk$message
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
    result$stop_reason <- chunk$delta$stop_reason
    result$stop_sequence <- chunk$delta$stop_sequence
    result$usage$output_tokens <- chunk$usage$output_tokens
  } else if (chunk$type == "error") {
    cli::cli_abort("{chunk$error$message}")
  } else {
    cli::cli_inform(c("!" = "Unknown chunk type {.str {chunk$type}}."))
  }
  result
}
method(stream_turn, ProviderClaude) <- function(provider, result) {
  contents <- lapply(result$content, function(content) {
    if (content$type == "text") {
      ContentText(content$text)
    } else if (content$type == "tool_use") {
      ContentToolRequest(content$id, content$name, content$input)
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  })

  tokens <- c(result$usage[[1]], result$usage[[2]])
  Turn(result$role, contents, json = result, tokens = tokens)
}
method(value_turn, ProviderClaude) <- method(stream_turn, ProviderClaude)


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

method(claude_content, ContentText) <- function(content) {
  list(type = "text", text = content@text)
}

method(claude_content, ContentImageRemote) <- function(content) {
  cli::cli_abort("Claude doesn't support remote images")
}

method(claude_content, ContentImageInline) <- function(content) {
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
method(claude_content, ContentToolRequest) <- function(content) {
  list(
    type = "tool_use",
    id = content@id,
    name = content@name,
    input = content@arguments
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(claude_content, ContentToolResult) <- function(content) {
  list(
    type = "tool_result",
    tool_use_id = content@id,
    content = tool_string(content),
    is_error = tool_errored(content)
  )
}

claude_tool <- function(tool) {
  list(
    name = tool@name,
    description = tool@description,
    input_schema = compact(json_schema_parameters(tool@arguments))
  )
}
