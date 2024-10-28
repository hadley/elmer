#' @include provider.R
#' @include content.R
#' @include types.R
NULL

#' Chat with an OpenAI model
#'
#' @description
#' [OpenAI](https://openai.com/) provides a number of chat-based models,
#' mostly under the [ChatGPT](https://chat.openai.com/) brand.
#' Note that a ChatGPT Plus membership does not grant access to the API.
#' You will need to sign up for a developer account (and pay for it) at the
#' [developer platform](https://platform.openai.com).
#'
#' For authentication, we recommend saving your
#' [API key](https://platform.openai.com/account/api-keys) to
#' the `OPENAI_API_KEY` environment variable in your `.Renviron` file.
#' You can easily edit this file by calling `usethis::edit_r_environ()`.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param turns A list of turns to start the chat with (i.e., continuing a
#'   previous conversation). If not provided, the conversation begins from
#'   scratch. Do not provide non-`NULL` values for both `turns` and
#'   `system_prompt`.
#'
#'   Each message in the list should be a named list with at least `role`
#'   (usually `system`, `user`, or `assistant`, but `tool` is also possible).
#'   Normally there is also a `content` field, which is a string.
#' @param base_url The base URL to the endpoint; the default uses OpenAI.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
#'   variable.
#' @param model The model to use for the chat. The default, `NULL`, will pick
#'   a reasonable default, and tell you about. We strongly recommend explicitly
#'   choosing a model for all but the most casual use.
#' @param seed Optional integer seed that ChatGPT uses to try and make output
#'   more reproducible.
#' @param api_args Named list of arbitrary extra arguments appended to the body
#'   of every chat API call.
#' @param echo One of the following options:
#'   * `none`: don't emit any output (default when running in a function).
#'   * `text`: echo text output as it streams in (default when running at
#'     the console).
#'   * `all`: echo all input and output.
#'
#'   Note this only affects the `chat()` method.
#' @family chatbots
#' @export
#' @returns A [Chat] object.
#' @examplesIf elmer:::openai_key_exists()
#' chat <- chat_openai()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
chat_openai <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = c("none", "text", "all")) {
  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "gpt-4o-mini")
  echo <- check_echo(echo)

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- ProviderOpenAI(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderOpenAI <- new_class(
  "ProviderOpenAI",
  parent = Provider,
  package = "elmer",
  properties = list(
    api_key = prop_string(),
    model = prop_string(),
    seed = prop_number_whole(allow_null = TRUE)
  )
)

openai_key_exists <- function() {
  key_exists("OPENAI_API_KEY")
}

openai_key <- function() {
  key_get("OPENAI_API_KEY")
}

# HTTP request and response handling -------------------------------------

# https://platform.openai.com/docs/api-reference/chat/create
method(chat_request, ProviderOpenAI) <- function(provider,
                                                 stream = TRUE,
                                                 turns = list(),
                                                 tools = list(),
                                                 spec = NULL,
                                                 extra_args = list()) {

  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_auth_bearer_token(req, provider@api_key)
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      resp_body_json(resp)$error$message
    }
  })

  messages <- openai_messages(turns)
  tools <- unname(lapply(tools, openai_tool, provider = provider))
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  if (!is.null(spec)) {
    response_format <- list(
      type = "json_schema",
      json_schema = list(
        name = "structured_data",
        schema = as_json(provider, spec),
        strict = TRUE
      )
    )
  } else {
    response_format <- NULL
  }

  data <- compact(list2(
    messages = messages,
    model = provider@model,
    seed = provider@seed,
    stream = stream,
    stream_options = if (stream) list(include_usage = TRUE),
    tools = tools,
    response_format = response_format,
    !!!extra_args
  ))
  req <- req_body_json(req, data)

  req
}

method(stream_parse, ProviderOpenAI) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  }

  if (identical(event$data, "[DONE]")) {
    return(NULL)
  }

  jsonlite::parse_json(event$data)
}
method(stream_text, ProviderOpenAI) <- function(provider, event) {
  if (length(event$choices) == 0) {
    NULL
  } else {
    event$choices[[1]]$delta$content
  }

}
method(stream_merge_chunks, ProviderOpenAI) <- function(provider, result, chunk) {
  if (is.null(result)) {
    chunk
  } else {
    merge_dicts(result, chunk)
  }
}
method(stream_turn, ProviderOpenAI) <- function(provider, result, has_spec = FALSE) {
  openai_assistant_turn(provider, result$choices[[1]]$delta, result, has_spec)
}
method(value_turn, ProviderOpenAI) <- function(provider, result, has_spec = FALSE) {
  openai_assistant_turn(provider, result$choices[[1]]$message, result, has_spec)
}
openai_assistant_turn <- function(provider, message, result, has_spec) {
  if (has_spec) {
    json <- jsonlite::parse_json(message$content[[1]])
    content <- list(ContentJson(json))
  } else {
    content <- lapply(message$content, as_content)
  }
  if (has_name(message, "tool_calls")) {
    calls <- lapply(message$tool_calls, function(call) {
      name <- call$`function`$name
      # TODO: record parsing error
      args <- jsonlite::parse_json(call$`function`$arguments)
      ContentToolRequest(name = name, arguments = args, id = call$id)
    })
    content <- c(content, calls)
  }
  tokens <- c(
    result$usage$prompt_tokens %||% NA_integer_,
    result$usage$completion_tokens %||% NA_integer_
  )
  tokens_log(paste0("OpenAI-", provider@base_url), tokens)

  Turn(message$role, content, json = result, tokens = tokens)
}

# Convert elmer turns + content to chatGPT messages ----------------------------
openai_messages <- function(turns) {
  messages <- list()
  add_message <- function(role, ...) {
    messages[[length(messages) + 1]] <<- compact(list(role = role, ...))
  }

  for (turn in turns) {
    if (turn@role == "system") {
      add_message("system", content = turn@contents[[1]]@text)
    } else if (turn@role == "user") {
      # Each tool result needs to go in its own message with role "tool"
      is_tool <- map_lgl(turn@contents, S7_inherits, ContentToolResult)

      content <- lapply(turn@contents[!is_tool], openai_content)
      if (length(content) > 0) {
        add_message("user", content = content)
      }
      for (tool in turn@contents[is_tool]) {
        add_message("tool", content = tool_string(tool), tool_call_id = tool@id)
      }
    } else if (turn@role == "assistant") {
      # Tool requests come out of content and go into own argument
      is_tool <- map_lgl(turn@contents, S7_inherits, ContentToolRequest)
      content <- lapply(turn@contents[!is_tool], openai_content)
      tool_calls <- lapply(turn@contents[is_tool], openai_content)

      add_message("assistant", content = content, tool_calls = tool_calls)
    } else {
      cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
    }
  }
  messages
}

openai_content <- new_generic("openai_content", "content")

method(openai_content, ContentText) <- function(content) {
  list(type = "text", text = content@text)
}

method(openai_content, ContentImageRemote) <- function(content) {
  list(type = "image_url", image_url = list(url = content@url))
}

method(openai_content, ContentImageInline) <- function(content) {
  list(
    type = "image_url",
    image_url = list(
      url = paste0("data:", content@type, ";base64,", content@data)
    )
  )
}

method(openai_content, ContentToolRequest) <- function(content) {
  json_args <- jsonlite::toJSON(content@arguments)
  list(
    id = content@id,
    `function` = list(name = content@name, arguments = json_args),
    type = "function"
  )
}

method(openai_content, ContentJson) <- function(content) {
  list(type = "text", text = "")
}

openai_tool <- function(provider, tool) {
  list(
    type = "function",
    "function" = compact(list(
      name = tool@name,
      description = tool@description,
      strict = TRUE,
      parameters = as_json(provider, tool@arguments)
    ))
  )
}


method(as_json, list(ProviderOpenAI, TypeObject)) <- function(provider, x) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for OpenAI.")
  }

  names <- names2(x@properties)
  properties <- lapply(x@properties, function(x) {
    out <- as_json(provider, x)
    if (!x@required) {
      out$type <- c(out$type, "null")
    }
    out
  })

  names(properties) <- names

  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names),
    additionalProperties = x@additional_properties
  )
}
