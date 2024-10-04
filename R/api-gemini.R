#' @include api.R
#' @include content.R
NULL

#' Create a chatbot that speaks to Google's Gemini
#'
#' This function returns a [Chat] object that takes care of managing the state
#' associated with the chat; i.e. it records the messages that you send to the
#' server, and the messages that you receive back. If you register a tool
#' (i.e. an R function that the assistant can call on your behalf), it also
#' takes care of the tool loop.
#'
#' @inheritParams new_chat_openai
#' @export
#' @returns A [Chat] object.
new_chat_gemini <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://generativelanguage.googleapis.com/v1beta/",
                            api_key = gemini_key(),
                            model = NULL,
                            api_args = list(),
                            echo = FALSE) {
  turns <- normalize_turns(turns, system_prompt)
  check_bool(echo)
  model <- set_default(model, "gemini-1.5-flash")

  provider <- new_gemini_provider(
    base_url = base_url,
    model = model,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

new_gemini_provider <- function(base_url = "https://generativelanguage.googleapis.com/v1beta/",
                                model = NULL,
                                extra_args = list(),
                                api_key = gemini_key(),
                                error_call = caller_env()) {

  # These checks could/should be placed in the validator, but the S7 object is
  # currently an implementation detail. Keeping these errors here avoids
  # leaking that implementation detail to the user.

  check_string(base_url, call = error_call)
  check_string(model, call = error_call)
  # check_named_list(extra_args, call = error_call())
  check_string(api_key, call = error_call)

  gemini_provider(
    base_url = base_url,
    model = model,
    api_key = api_key,
    extra_args = extra_args
  )
}

gemini_provider <- new_class(
  "gemini_provider",
  package = "elmer",
  properties = list(
    base_url = class_character,
    model = class_character,
    api_key = class_character,
    extra_args = class_list
  )
)

gemini_key <- function() {
  key_get("GOOGLE_API_KEY")
}

# HTTP request and response handling -------------------------------------

# https://platform.openai.com/docs/api-reference/chat/create
method(chat_request, gemini_provider) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  extra_args = list()) {


  req <- request(provider@base_url)
  req <- req_headers(req, "x-goog-api-key" = provider@api_key, .redact = "x-goog-api-key")
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) {
    json <- resp_body_json(resp, check_type = FALSE)
    json$error$message
  })

  req <- req_url_path_append(req, "models")
  if (stream) {
    # https://ai.google.dev/api/generate-content
    req <- req_url_path_append(req, paste0(provider@model, ":", "streamGenerateContent"))
    req <- req_url_query(req, alt = "sse")
  } else {
    # https://ai.google.dev/api/generate-content
    req <- req_url_path_append(req, paste0(provider@model, ":", "generateContent"))
  }

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- list(parts = list(text = turns[[1]]@text))
  } else {
    system <- list(parts = list(text = ""))
  }

  contents <- gemini_contents(turns)
  tools <- gemini_tools(tools)

  body <- compact(list(
    contents = contents,
    tools = tools,
    systemInstruction = system
  ))
  req <- req_body_json(req, body)

  req
}

method(stream_is_done, gemini_provider) <- function(provider, event) {
  is.null(event)
}
method(stream_parse, gemini_provider) <- function(provider, event) {
  jsonlite::parse_json(event$data)
}
method(stream_text, gemini_provider) <- function(provider, event) {
  event$candidates[[1]]$content$parts[[1]]$text
}
method(stream_merge_chunks, gemini_provider) <- function(provider, result, chunk) {
  if (is.null(result)) {
    chunk
  } else {
    merge_dicts(result, chunk)
  }
}
method(stream_turn, gemini_provider) <- function(provider, result) {
  gemini_assistant_turn(result$candidates[[1]]$content)
}
method(value_turn, gemini_provider) <- function(provider, result) {
  gemini_assistant_turn(result$candidates[[1]]$content)
}
gemini_assistant_turn <- function(message) {
  content <- lapply(message$parts, function(content) {
    if (hasName(content, "text")) {
      content_text(content$text)
    } else if (hasName(content, "functionCall")) {
      content_tool_request(
        content$functionCall$name,
        content$functionCall$name,
        content$functionCall$args
      )
    } else {
      browser()
    }
  })

  turn("assistant", content)
}

# Convert elmer turns + content to chatGPT messages

# https://ai.google.dev/api/caching#Content
gemini_contents <- function(turns) {
  compact(lapply(turns, function(turn) {
    if (turn@role == "system") {
      # System messages go in the top-level API parameter
    } else if (turn@role == "user") {
      parts <- lapply(turn@content, gemini_content)
      list(role = turn@role, parts = parts)
    } else if (turn@role == "assistant") {
      parts <- lapply(turn@content, gemini_content)
      list(role = "model", parts = parts)
    } else {
      cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
    }
  }))
}

# https://ai.google.dev/api/caching#Tool
gemini_tools <- function(tools) {
  if (length(tools) == 0) {
    return()
  }

  funs <- lapply(tools, function(tool) {
    compact(list(
      name = tool$`function`$name,
      description = tool$`function`$description,
      parameters = openapi_schema_parameters(tool$`function`$parameters)
    ))
  })
  list(functionDeclarations = funs)
}

# https://ai.google.dev/api/caching#Schema
openapi_schema_parameters <- function(parameters) {
  # TODO: Add required
  if (length(parameters$properties) == 0) {
    return(list())
  }
  properties <- lapply(parameters$properties, function(prop) {
    list(
      type = prop$type,
      description = prop$description
    )
  })
  list(
    type = "object",
    properties = properties
  )
}

gemini_content <- new_generic("gemini_content", "content")

method(gemini_content, content_text) <- function(content) {
  list(text = content@text)
}

# https://ai.google.dev/api/caching#FileData
method(gemini_content, content_image_remote) <- function(content) {
  cli::cli_abort("Gemini doesn't support remote images")
}

# https://ai.google.dev/api/caching#Blob
method(gemini_content, content_image_inline) <- function(content) {
  list(
    inlineData = list(
      mimeType = content@type,
      data = content@data
    )
  )
}

# https://ai.google.dev/api/caching#FunctionCall
method(gemini_content, content_tool_request) <- function(content) {
  list(
    functionCall = list(
      name = content@id,
      args = content@arguments
    )
  )
}

# https://ai.google.dev/api/caching#FunctionResponse
method(gemini_content, content_tool_result) <- function(content) {
  if (is.null(content@result)) {
    result <- paste0("Tool calling failed with error ", content@error)
  } else {
    result <- toString(content@result)
  }

  list(
    functionResponse = list(
      name = content@id,
      response = list(value = result)
    )
  )
}
