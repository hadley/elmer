
#' Create a chatbot that speaks to an OpenAI compatible endpoint
#'
#' This function returns a [Chat] object that takes care of managing the state
#' associated with the chat; i.e. it records the messages that you send to the
#' server, and the messages that you receive back. If you register a tool
#' (aka an R function), it also takes care of the tool loop.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param messages A list of messages to start the chat with (i.e., continuing a
#'   previous conversation). If not provided, the conversation begins from
#'   scratch. Do not provide non-`NULL` values for both `messages` and
#'   `system_prompt`.
#'
#'   Each message in the list should be a named list with at least `role`
#'   (usually `system`, `user`, or `assistant`, but `tool` is also possible).
#'   Normally there is also a `content` field, which is a string.
#' @param base_url The base URL to the endpoint; the default uses OpenAI.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
#'   variable.
#' @param model The model to use for the chat; set to `NULL` (the default) to
#'   use a reasonable model, currently `gpt-4o-mini`. We strongly recommend
#'   explicitly choosing a model for all but the most casual use.
#' @param seed Optional integer seed that ChatGPT uses to try and make output
#'   more reproducible.
#' @param api_args Named list of arbitrary extra arguments passed to every
#'   chat API call.
#' @param echo If `TRUE`, the `chat()` method streams the response to stdout by
#'   default. (Note that this has no effect on the `stream()`, `chat_async()`,
#'   and `stream_async()` methods.)
#' @export
#' @value A [Chat] object
#' @examplesIf elmer:::openai_key_exists()
#' chat <- new_chat_openai()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
#'
#' chat <- new_chat_openai()
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
new_chat_openai <- function(system_prompt = NULL,
                            messages = NULL,
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = FALSE) {
  check_string(system_prompt, allow_null = TRUE)
  openai_check_conversation(messages, allow_null = TRUE)
  check_string(base_url)
  check_string(api_key)
  check_string(model, allow_null = TRUE, allow_na = TRUE)
  check_number_decimal(seed, allow_null = TRUE)
  check_bool(echo)

  model <- model %||% "gpt-4o-mini"
  if (is_testing() && is.null(seed)) {
    seed <- 1014
  }

  model <- openai_model(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )

  messages <- openai_apply_system_prompt(system_prompt, messages)
  Chat$new(model = model, messages = messages, echo = echo)
}


openai_apply_system_prompt <- function(system_prompt, messages) {
  if (is.null(system_prompt)) {
    return(messages)
  }

  system_prompt_message <- list(
    role = "system",
    content = system_prompt
  )

  # No messages; start with just the system prompt
  if (length(messages) == 0) {
    return(list(system_prompt_message))
  }

  # No existing system prompt message; prepend the new one
  if (messages[[1]][["role"]] != "system") {
    return(c(list(system_prompt_message), messages))
  }

  # Duplicate system prompt; return as-is
  if (messages[[1]][["content"]] == system_prompt) {
    return(messages)
  }

  cli::cli_abort("`system_prompt` and `messages[[1]]` contained conflicting system prompts")
}

openai_check_conversation <- function(messages, allow_null = FALSE) {
  if (is.null(messages) && isTRUE(allow_null)) {
    return()
  }

  if (!is.list(messages) ||
      !(is.null(names(messages)) || all(names(messages) == ""))) {
    stop_input_type(
      messages,
      "an unnamed list of messages",
      allow_null = FALSE
    )
  }

  for (message in messages) {
    if (!is.list(message) ||
        !is.character(message$role)) {
      cli::cli_abort("Each message must be a named list with at least a `role` field.")
    }
  }
}


openai_model <- function(base_url = "https://api.openai.com/v1",
                         model = "gpt-4o-mini",
                         seed = NULL,
                         extra_args = list(),
                         api_key = openai_key()) {
  structure(
    list(
      base_url = base_url,
      model = model,
      seed = seed,
      api_key = api_key,
      extra_args = list()
    ),
    class = "elmer::openai_model"
  )
}


openai_chat <- function(mode = c("value", "stream", "async-stream", "async-value"),
                        model,
                        messages,
                        tools = list()) {

  mode <- arg_match(mode)
  stream <- mode %in% c("stream", "async-stream")

  req <- openai_chat_req(
    messages = messages,
    tools = tools,
    model = model$model,
    seed = model$seed,
    stream = stream,
    base_url = model$base_url,
    api_key = model$api_key
  )

  handle_response <- switch(mode,
    "value" = function(req) resp_body_json(req_perform(req)),
    "async-value" = function(req) promises::then(req_perform_promise(req), resp_body_json),
    "stream" = openai_chat_stream,
    "async-stream" = openai_chat_stream_async
  )

  handle_response(req)
}

openai_chat_is_done <- function(event) {
  identical(event$data, "[DONE]")
}

openai_chat_parse <- function(event) {
  jsonlite::parse_json(event$data)
}

on_load(openai_chat_stream <- chat_stream(openai_chat_is_done, openai_chat_parse))
on_load(openai_chat_stream_async <- chat_stream_async(openai_chat_is_done, openai_chat_parse))

openai_chunk_text <- function(event, streaming) {
  if (streaming) {
    event$choices[[1]]$delta$content
  } else {
    event$choices[[1]]$message$content
  }
}
openai_merge_chunks <- function(cur, chunk) {
  if (is.null(cur)) {
    chunk
  } else {
    merge_dicts(cur, chunk)
  }
}
openai_result_message <- function(result, streaming) {
  if (streaming) {
    result$choices[[1]]$delta
  } else {
    result$choices[[1]]$message
  }
}

# https://platform.openai.com/docs/api-reference/chat/create
openai_chat_req <- function(messages,
                            tools = list(),
                            model = "gpt-4o-mini",
                            seed = NULL,
                            stream = TRUE,
                            extra_args = list(),
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key()) {

  check_string(model)
  check_number_whole(seed, allow_null = TRUE)
  check_bool(stream)

  data <- compact(list2(
    messages = messages,
    model = model,
    seed = seed,
    stream = stream,
    tools = tools,
    !!!extra_args
  ))

  req <- openai_request(base_url = base_url, key = api_key)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_body_json(req, data)
  req
}

openai_request <- function(base_url = "https://api.openai.com/v1",
                           key = openai_key()) {
  req <- request(base_url)
  req <- req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) {
     resp_body_json(resp)$error$message
  })
  # req <- req_verbose(req, body_req = TRUE, body_resp = TRUE)
  req
}

openai_key_exists <- function() {
  key_exists("OPENAI_API_KEY")
}

openai_key <- function() {
  key_get("OPENAI_API_KEY")
}
