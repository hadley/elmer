#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Chat with an Anthropic Claude model
#'
#' @description
#' [Anthropic](https://www.anthropic.com) provides a number of chat based
#' models under the [Claude](https://www.anthropic.com/claude) moniker.
#' Note that a Claude Pro membership does not give you the ability to call
#' models via the API; instead, you will need to sign up (and pay for) a
#' [developer account](https://console.anthropic.com/)
#'
#' To authenticate, we recommend saving your
#' [API key](https://console.anthropic.com/account/keys) to
#' the `ANTHROPIC_API_KEY` env var in your `.Renviron`
#' (which you can easily edit by calling `usethis::edit_r_environ()`).
#'
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `ANTHROPIC_API_KEY` environment
#'   variable.
#' @param max_tokens Maximum number of tokens to generate before stopping.
#' @family chatbots
#' @export
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

  model <- model %||% "claude-3-5-sonnet-latest"

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
  properties = list(
    api_key = prop_string(),
    model = prop_string(),
    max_tokens = prop_number_whole(min = 1)
  )
)

anthropic_key <- function() {
  key_get("ANTHROPIC_API_KEY")
}
anthropic_key_exists <- function() {
  key_exists("ANTHROPIC_API_KEY")
}

method(chat_request, ProviderClaude) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  type = NULL,
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
  # 529 is not documented, but we see it fairly frequently in tests
  req <- req_retry(
    req,
    max_tries = 2,
    is_transient = function(resp) resp_status(resp) %in% c(429, 503, 529)
  )

  # <https://docs.anthropic.com/en/api/errors>
  req <- req_error(req, body = function(resp) {
    json <- resp_body_json(resp)
    paste0(json$error$message, " [", json$error$type, "]")
  })

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system <- NULL
  }

  messages <- compact(as_json(provider, turns))

  if (!is.null(type)) {
    tool_def <- ToolDef(
      fun = function(...) {},
      name = "_structured_tool_call",
      description = "Extract structured data",
      arguments = type_object(data = type)
    )
    tools[[tool_def@name]] <- tool_def
    tool_choice <- list(type = "tool", name = tool_def@name)
    stream <- FALSE
  } else {
    tool_choice <- NULL
  }
  tools <- as_json(provider, unname(tools))

  extra_args <- utils::modifyList(provider@extra_args, extra_args)
  body <- compact(list2(
    model = provider@model,
    system = system,
    messages = messages,
    stream = stream,
    max_tokens = provider@max_tokens,
    tools = tools,
    tool_choice = tool_choice,
    !!!extra_args
  ))
  req <- req_body_json(req, body)

  req
}

# Claude -> elmer --------------------------------------------------------------

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
    if (chunk$delta$type == "text_delta") {
      paste(result$content[[chunk$index + 1L]]$text) <- chunk$delta$text
    } else if (chunk$delta$type == "input_json_delta") {
      paste(result$content[[chunk$index + 1L]]$input) <- chunk$delta$partial_json
    } else {
      cli::cli_inform(c("!" = "Unknown delta type {.str {chunk$delta$type}}."))
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

method(value_turn, ProviderClaude) <- function(provider, result, has_type = FALSE) {
  contents <- lapply(result$content, function(content) {
    if (content$type == "text") {
      ContentText(content$text)
    } else if (content$type == "tool_use") {
      if (has_type) {
        ContentJson(content$input$data)
      } else {
        if (is_string(content$input)) {
          content$input <- jsonlite::parse_json(content$input)
        }
        ContentToolRequest(content$id, content$name, content$input)
      }
    } else {
      cli::cli_abort(
        "Unknown content type {.str {content$type}}.",
        .internal = TRUE
      )
    }
  })

  tokens <- c(result$usage[[1]], result$usage[[2]])
  tokens_log("Claude", tokens)

  Turn(result$role, contents, json = result, tokens = tokens)
}

# elmer -> Claude --------------------------------------------------------------

method(as_json, list(ProviderClaude, Turn)) <- function(provider, x) {
  if (x@role == "system") {
    # claude passes system prompt as separate arg
    NULL
  } else if (x@role %in% c("user", "assistant")) {
    list(role = x@role, content = as_json(provider, x@contents))
  } else {
    cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderClaude, ContentText)) <- function(provider, x) {
  list(type = "text", text = x@text)
}

method(as_json, list(ProviderClaude, ContentImageRemote)) <- function(provider, x) {
  cli::cli_abort("Claude doesn't support remote images")
}

method(as_json, list(ProviderClaude, ContentImageInline)) <- function(provider, x) {
  list(
    type = "image",
    source = list(
      type = "base64",
      media_type = x@type,
      data = x@data
    )
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(as_json, list(ProviderClaude, ContentToolRequest)) <- function(provider, x) {
  list(
    type = "tool_use",
    id = x@id,
    name = x@name,
    input = x@arguments
  )
}

# https://docs.anthropic.com/en/docs/build-with-claude/tool-use#handling-tool-use-and-tool-result-content-blocks
method(as_json, list(ProviderClaude, ContentToolResult)) <- function(provider, x) {
  list(
    type = "tool_result",
    tool_use_id = x@id,
    content = tool_string(x),
    is_error = tool_errored(x)
  )
}

method(as_json, list(ProviderClaude, ToolDef)) <- function(provider, x) {
  list(
    name = x@name,
    description = x@description,
    input_schema = compact(as_json(provider, x@arguments))
  )
}
