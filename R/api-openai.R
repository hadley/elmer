#' @include api.R
NULL

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
  check_bool(echo)

  model <- set_default(model, "gpt-4o-mini")

  model <- new_openai_model(
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

new_openai_model <- function(base_url = "https://api.openai.com/v1",
                             model = NULL,
                             seed = NULL,
                             extra_args = list(),
                             api_key = openai_key(),
                             error_call = caller_env()) {

  # These checks could/should be placed in the validator, but the S7 object is
  # currently an implementation detail. Keeping these errors here avoids
  # leaking that implementation detail to the user.

  check_string(base_url, call = error_call())
  check_string(model, call = error_call())
  check_number_whole(seed, allow_null = TRUE, call = error_call())
  # check_named_list(extra_args, call = error_call())
  check_string(api_key, call = error_call())

  if (is_testing() && is.null(seed)) {
    seed <- 1014
  }

  openai_model(
    base_url = base_url,
    model = model,
    seed = seed,
    api_key = api_key,
    extra_args = list()
  )
}

openai_model <- new_class(
  "openai_model",
  package = "elmer",
  properties = list(
    base_url = class_character,
    model = class_character,
    seed = class_double | NULL,
    api_key = class_character,
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
method(chat_request, openai_model) <- function(model,
                                               stream = TRUE,
                                               messages = list(),
                                               tools = list(),
                                               extra_args = list()) {

  req <- request(model@base_url)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_auth_bearer_token(req, model@api_key)
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) resp_body_json(resp)$error$message)

  extra_args <- utils::modifyList(model@extra_args, extra_args)

  data <- compact(list2(
    messages = messages,
    model = model@model,
    seed = model@seed,
    stream = stream,
    tools = tools,
    !!!extra_args
  ))
  req <- req_body_json(req, data)

  req
}

method(stream_is_done, openai_model) <- function(model, event) {
  identical(event$data, "[DONE]")
}
method(stream_parse, openai_model) <- function(model, event) {
  jsonlite::parse_json(event$data)
}
method(stream_text, openai_model) <- function(model, event) {
  event$choices[[1]]$delta$content
}
method(stream_merge_chunks, openai_model) <- function(model, result, chunk) {
  if (is.null(result)) {
    chunk
  } else {
    merge_dicts(result, chunk)
  }
}
method(stream_message, openai_model) <- function(model, result) {
  result$choices[[1]]$delta
}

method(value_text, openai_model) <- function(model, event) {
  event$choices[[1]]$message$content
}
method(value_message, openai_model) <- function(model, result) {
  result$choices[[1]]$message
}

method(value_tool_calls, openai_model) <- function(model, message, tools) {
  lapply(message$tool_calls, function(call) {
    fun <- tools[[call$`function`$name]]
    args <- jsonlite::parse_json(call$`function`$arguments)
    list(fun = fun, args = args, id = call$id)
  })
}

method(call_tools, openai_model) <- function(model, tool_calls) {
  lapply(tool_calls, function(call) {
    result <- call_tool(call$fun, call$args)

    if (promises::is.promise(result)) {
      cli::cli_abort(c(
        "Can't use async tools with `$chat()` or `$stream()`.",
        i = "Async tools are supported, but you must use `$chat_async()` or `$stream_async()`."
      ))
    }

    openai_tool_result(result, call$id)
  })
}

rlang::on_load(
  method(call_tools_async, openai_model) <- coro::async(function(model, tool_calls) {
    # We call it this way instead of a more natural for + await_each() because
    # we want to run all the async tool calls in parallel
    result_promises <- lapply(tool_calls, function(call) {
      promises::then(
        call_tool_async(call$fun, call$args),
        function(result) openai_tool_result(result, id = call$id)
      )
    })

    promises::promise_all(.list = result_promises)
  })
)

openai_tool_result <- function(result, id) {
  list(
    role = "tool",
    content = toString(result),
    tool_call_id = id
  )
}
