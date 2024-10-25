#' @examples
#'
#' chat_bedrock()$chat("Who are you?")
chat_bedrock <- function(system_prompt = NULL,
                         turns = NULL,
                         model = NULL,
                         profile = NULL,
                         echo = NULL) {

  check_installed("paws.common", "AWS authentication")
  credentials <- paws.common::locate_credentials(profile)

  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "anthropic.claude-3-5-sonnet-20240620-v1:0")
  echo <- check_echo(echo)

  provider <- ProviderBedrock(
    base_url = "",
    model = model,
    profile = profile,
    credentials = credentials
  )

  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderBedrock <- new_class(
  "ProviderBedrock",
  parent = Provider,
  package = "elmer",
  properties = list(
    model = prop_string(),
    profile = prop_string(allow_null = TRUE),
    credentials = class_list
  )
)

method(chat_request, ProviderBedrock) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  spec = NULL,
                                                  extra_args = list()) {

  req <- request(paste0(
    "https://bedrock-runtime.", provider@credentials$region, ".amazonaws.com"
  ))
  req <- req_url_path_append(
    req,
    "model",
    provider@model,
    if (stream) "converse-stream" else "converse"
  )
  req <- req_auth_aws_v4(
    req,
    aws_access_key_id = provider@credentials$access_key_id,
    aws_secret_access_key = provider@credentials$secret_access_key,
    aws_session_token = provider@credentials$session_token
  )

  req <- req_error(req, body = function(resp) {
    body <- resp_body_json(resp)
    body$Message
  })

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system <- NULL
  }

  messages <- claude_messages(turns)
  # tools <- unname(lapply(tools, bedrock_tool))

  # https://docs.aws.amazon.com/bedrock/latest/APIReference/API_runtime_Converse.html
  req <- req_body_json(req, list(
    messages = messages,
    system = list(list(text = system))
  ))

  req
}

method(chat_resp_stream, ProviderBedrock) <- function(provider, resp) {
  resp_stream_aws(resp)
}

method(stream_parse, ProviderBedrock) <- function(provider, event) {
  if (is.null(event)) {
    return()
  }

  body <- event$body
  body$event_type <- event$headers$`:event-type`
  body$p <- NULL # padding?

  body
}
method(stream_text, ProviderBedrock) <- function(provider, event) {
  if (event$event_type == "contentBlockDelta") {
    event$delta$text
  }
}
method(stream_merge_chunks, ProviderBedrock) <- function(provider, result, chunk) {
  if (chunk$event_type == "messageStart") {
    result <- list(role = chunk$role, content = list())
  } else if (chunk$event_type == "contentBlockDelta") {
    i <- chunk$contentBlockIndex + 1
    if (i > length(result$content)) {
      result$content[[i]] <- list(text = chunk$delta$text)
    } else {
      paste(result$content[[i]]$text) <- chunk$delta$text
    }
  } else if (chunk$event_type == "contentBlockStop") {
    # don't need to do anything?
  } else if (chunk$event_type == "messageStop") {
    # match structure of non-streaming
    result <- list(
      output = list(
        message = result
      )
    )
  } else if (chunk$event_type == "metadata") {
    result$usage <- chunk$usage
    result$metrics <- chunk$metrics
  } else {
    browser()
    cli::cli_inform(c("!" = "Unknown chunk type {.str {event_type}}."))
  }

  result
}

method(stream_turn, ProviderBedrock) <- function(provider, result, has_spec = FALSE) {
  contents <- lapply(result$output$message$content, function(content) {
    if (has_name(content, "text")) {
      ContentText(content$text)
    } else {
      browsers()
      cli::cli_abort(
        "Unknown content type {.str {names(content)}}.",
        .internal = TRUE
      )
    }
  })

  tokens <- c(result$usage$inputTokens, result$usage$outputTokens)
  tokens_log("Bedrock", tokens)

  Turn(result$output$message$role, contents, json = result, tokens = tokens)
}
method(value_turn, ProviderBedrock) <- method(stream_turn, ProviderBedrock)


# Helpers ----------------------------------------------------------------

has_paws_credentials <- function() {
  tryCatch(
    {
      paws.common::locate_credentials()
      TRUE
    },
    error = function(e) {
      FALSE
    }
  )
}
