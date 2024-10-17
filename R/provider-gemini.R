#' @include provider.R
#' @include content.R
#' @include types.R
NULL

#' Chat with a Google Gemini model
#'
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `GOOGLE_API_KEY` environment
#'   variable.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
chat_gemini <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://generativelanguage.googleapis.com/v1beta/",
                            api_key = gemini_key(),
                            model = NULL,
                            api_args = list(),
                            echo = NULL) {
  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "gemini-1.5-flash")
  echo <- check_echo(echo)

  provider <- ProviderGemini(
    base_url = base_url,
    model = model,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderGemini <- new_class(
  "ProviderGemini",
  parent = Provider,
  package = "elmer",
  properties = list(
    api_key = prop_string(),
    model = prop_string()
  )
)

gemini_key <- function() {
  key_get("GOOGLE_API_KEY")
}

# HTTP request and response handling -------------------------------------

method(chat_request, ProviderGemini) <- function(provider,
                                                 stream = TRUE,
                                                 turns = list(),
                                                 tools = list(),
                                                 spec = NULL,
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
    # https://ai.google.dev/api/generate-content#method:-models.streamgeneratecontent
    req <- req_url_path_append(req, paste0(provider@model, ":", "generateContent"))
  }

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- list(parts = list(text = turns[[1]]@text))
  } else {
    system <- list(parts = list(text = ""))
  }

  if (!is.null(spec)) {
    generation_config <- list(
      response_mime_type = "application/json",
      response_schema = as_json(provider, spec)
    )
  } else {
    generation_config <- NULL
  }

  contents <- gemini_contents(turns)
  tools <- gemini_tools(provider, tools)
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  body <- compact(list(
    contents = contents,
    tools = tools,
    systemInstruction = system,
    generation_config = generation_config,
    !!!extra_args
  ))
  req <- req_body_json(req, body)

  req
}

method(stream_parse, ProviderGemini) <- function(provider, event) {
  if (is.null(event)) {
    NULL
  } else {
    jsonlite::parse_json(event$data)
  }
}
method(stream_text, ProviderGemini) <- function(provider, event) {
  event$candidates[[1]]$content$parts[[1]]$text
}
method(stream_merge_chunks, ProviderGemini) <- function(provider, result, chunk) {
  if (is.null(result)) {
    chunk
  } else {
    merge_dicts(result, chunk)
  }
}
method(stream_turn, ProviderGemini) <- function(provider, result, has_spec = FALSE) {
  message <- result$candidates[[1]]$content

  contents <- lapply(message$parts, function(content) {
    if (has_name(content, "text")) {
      if (has_spec) {
        data <- jsonlite::parse_json(content$text)
        ContentJson(data)
      } else {
        ContentText(content$text)
      }
    } else if (has_name(content, "functionCall")) {
      ContentToolRequest(
        content$functionCall$name,
        content$functionCall$name,
        content$functionCall$args
      )
    } else {
      cli::cli_abort(
        "Unknown content type with names {.str {names(content)}}.",
        .internal = TRUE
      )
    }
  })
  usage <- result$usageMetadata
  tokens <- c(usage$promptTokenCount, usage$candidatesTokenCount)
  tokens_log("Gemini", tokens)

  Turn("assistant", contents, json = result, tokens = tokens)
}
method(value_turn, ProviderGemini) <- method(stream_turn, ProviderGemini)

# https://ai.google.dev/api/caching#Content
gemini_contents <- function(turns) {
  compact(lapply(turns, function(turn) {
    if (turn@role == "system") {
      # System messages go in the top-level API parameter
    } else if (turn@role == "user") {
      parts <- lapply(turn@contents, gemini_content)
      list(role = turn@role, parts = parts)
    } else if (turn@role == "assistant") {
      parts <- lapply(turn@contents, gemini_content)
      list(role = "model", parts = parts)
    } else {
      cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
    }
  }))
}

# https://ai.google.dev/api/caching#Tool
gemini_tools <- function(provider, tools) {
  if (length(tools) == 0) {
    return()
  }

  funs <- lapply(tools, function(tool) {
    compact(list(
      name = tool@name,
      description = tool@description,
      parameters = as_json(provider, tool@arguments)
    ))
  })
  list(functionDeclarations = unname(funs))
}

gemini_content <- new_generic("gemini_content", "content")

method(gemini_content, ContentText) <- function(content) {
  list(text = content@text)
}

# https://ai.google.dev/api/caching#FileData
method(gemini_content, ContentImageRemote) <- function(content) {
  cli::cli_abort("Gemini doesn't support remote images")
}

# https://ai.google.dev/api/caching#Blob
method(gemini_content, ContentImageInline) <- function(content) {
  list(
    inlineData = list(
      mimeType = content@type,
      data = content@data
    )
  )
}

# https://ai.google.dev/api/caching#FunctionCall
method(gemini_content, ContentToolRequest) <- function(content) {
  list(
    functionCall = list(
      name = content@id,
      args = content@arguments
    )
  )
}

# https://ai.google.dev/api/caching#FunctionResponse
method(gemini_content, ContentToolResult) <- function(content) {
  list(
    functionResponse = list(
      name = content@id,
      response = list(value = tool_string(content))
    )
  )
}

method(as_json, list(ProviderGemini, TypeObject)) <- function(provider, x) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for Gemini.")
  }

  if (length(x@properties) == 0) {
    return(list())
  }

  names <- names2(x@properties)
  required <- map_lgl(x@properties, function(prop) prop@required)

  properties <- lapply(x@properties, as_json, provider = provider)
  names(properties) <- names

  compact(list(
    type = "object",
    description = x@description,
    properties = properties,
    required = as.list(names[required])
  ))
}
