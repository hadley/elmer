#' @include api.R
#' @include content.R
NULL

#' Create a chatbot that speaks to an OpenAI compatible endpoint
#'
#' This function returns a [Chat] object that takes care of managing the state
#' associated with the chat; i.e. it records the messages that you send to the
#' server, and the messages that you receive back. If you register a tool
#' (i.e. an R function that the assistant can call on your behalf), it also
#' takes care of the tool loop.
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
#' @param api_args Named list of arbitrary extra arguments passed to every
#'   chat API call.
#' @param echo If `TRUE`, the `chat()` method streams the response to stdout by
#'   default. (Note that this has no effect on the `stream()`, `chat_async()`,
#'   and `stream_async()` methods.)
#' @export
#' @returns A [Chat] object.
#' @examplesIf elmer:::openai_key_exists()
#' chat <- chat_openai()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
#'
#' chat <- chat_openai()
#' chat$register_tool(
#'   fun = rnorm,
#'   name = "rnorm",
#'   description = "Drawn numbers from a random normal distribution",
#'   arguments = list(
#'     n = tool_arg(
#'       type = "integer",
#'       description = "The number of observations. Must be a positive integer."
#'     ),
#'     mean = tool_arg(
#'       type = "number",
#'       description = "The mean value of the distribution."
#'     ),
#'     sd = tool_arg(
#'       type = "number",
#'       description = "The standard deviation of the distribution. Must be a non-negative number."
#'     )
#'   )
#' )
#' chat$chat("
#'   Give me five numbers from a random normal distribution.
#'   Briefly explain your work.
#' ")
chat_openai <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = FALSE) {
  turns <- normalize_turns(turns, system_prompt)
  check_bool(echo)
  model <- set_default(model, "gpt-4o-mini")

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- openai_provider(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

openai_provider <- new_class(
  "openai_provider",
  package = "elmer",
  properties = list(
    base_url = prop_string(),
    model = prop_string(),
    seed = prop_number_whole(allow_null = TRUE),
    api_key = prop_string(),
    extra_args = class_list
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
method(chat_request, openai_provider) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  extra_args = list()) {

  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_auth_bearer_token(req, provider@api_key)
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) resp_body_json(resp)$error$message)

  messages <- openai_messages(turns)
  tools <- lapply(tools, openai_tool)
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  data <- compact(list2(
    messages = messages,
    model = provider@model,
    seed = provider@seed,
    stream = stream,
    tools = tools,
    !!!extra_args
  ))
  req <- req_body_json(req, data)

  req
}

method(stream_is_done, openai_provider) <- function(provider, event) {
  if (is.null(event)) {
    cli::cli_abort("Connection closed unexpectedly")
  } else {
    identical(event$data, "[DONE]")
  }
}
method(stream_parse, openai_provider) <- function(provider, event) {
  jsonlite::parse_json(event$data)
}
method(stream_text, openai_provider) <- function(provider, event) {
  event$choices[[1]]$delta$content
}
method(stream_merge_chunks, openai_provider) <- function(provider, result, chunk) {
  if (is.null(result)) {
    chunk
  } else {
    merge_dicts(result, chunk)
  }
}
method(stream_turn, openai_provider) <- function(provider, result) {
  openai_assistant_turn(result$choices[[1]]$delta)
}
method(value_turn, openai_provider) <- function(provider, result) {
  openai_assistant_turn(result$choices[[1]]$message)
}
openai_assistant_turn <- function(message) {
  content <- lapply(message$content, as_content)

  if (has_name(message, "tool_calls")) {
    calls <- lapply(message$tool_calls, function(call) {
      name <- call$`function`$name
      # TODO: record parsing error
      args <- jsonlite::parse_json(call$`function`$arguments)
      content_tool_request(name = name, arguments = args, id = call$id)
    })
    content <- c(content, calls)
  }
  turn(
    role = message$role,
    content = content,
    extra = message["refusal"]
  )
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
      is_tool <- map_lgl(turn@contents, S7_inherits, content_tool_result)

      content <- lapply(turn@contents[!is_tool], openai_content)
      if (length(content) > 0) {
        add_message("user", content = content)
      }
      for (tool in turn@contents[is_tool]) {
        add_message("tool", content = openai_content(tool), tool_call_id = tool@id)
      }
    } else if (turn@role == "assistant") {
      # Tool requests come out of content and go into own argument
      is_tool <- map_lgl(turn@contents, S7_inherits, content_tool_request)
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

method(openai_content, content_text) <- function(content) {
  list(type = "text", text = content@text)
}

method(openai_content, content_image_remote) <- function(content) {
  list(type = "image_url", image_url = list(url = content@url))
}

method(openai_content, content_image_inline) <- function(content) {
  list(
    type = "image_url",
    image_url = list(
      url = paste0("data:", content@type, ";base64,", content@data)
    )
  )
}

method(openai_content, content_tool_result) <- function(content) {
  if (is.null(content@result)) {
    result <- paste0("Tool calling failed with error ", content@error)
  } else {
    result <- toString(content@result)
  }

  result
}

method(openai_content, content_tool_request) <- function(content) {
  json_args <- jsonlite::toJSON(content@arguments)
  list(
    id = content@id,
    `function` = list(name = content@name, arguments = json_args),
    type = "function"
  )
}

openai_tool <- function(tool) {
  list(
    type = "function",
    "function" = compact(list(
      name = tool@name,
      description = tool@description,
      strict = tool@extra$strict,
      parameters = json_schema_parameters(tool@arguments)
    ))
  )
}
