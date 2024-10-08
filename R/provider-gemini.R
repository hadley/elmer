#' @include provider.R
#' @include content.R
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

  contents <- gemini_contents(turns)
  tools <- gemini_tools(tools)
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  body <- compact(list(
    contents = contents,
    tools = tools,
    systemInstruction = system,
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
method(stream_turn, ProviderGemini) <- function(provider, result) {
  gemini_assistant_turn(result$candidates[[1]]$content, result)
}
method(value_turn, ProviderGemini) <- function(provider, result) {
  gemini_assistant_turn(result$candidates[[1]]$content, result)
}
gemini_assistant_turn <- function(message, result) {
  contents <- lapply(message$parts, function(content) {
    if (has_name(content, "text")) {
      ContentText(content$text)
    } else if (has_name(content, "functionCall")) {
      ContentToolRequest(
        content$functionCall$name,
        content$functionCall$name,
        content$functionCall$args
      )
    } else {
      browser()
    }
  })
  usage <- result$usageMetadata
  tokens <- c(usage$promptTokenCount, usage$candidatesTokenCount)
  tokens_log("Gemini", tokens)

  Turn("assistant", contents, json = result, tokens = tokens)
}

# Convert elmer turns + content to chatGPT messages

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
gemini_tools <- function(tools) {
  if (length(tools) == 0) {
    return()
  }

  funs <- lapply(tools, function(tool) {
    compact(list(
      name = tool@name,
      description = tool@description,
      parameters = openapi_schema_parameters(tool@arguments)
    ))
  })
  list(functionDeclarations = unname(funs))
}

# https://ai.google.dev/api/caching#Schema
openapi_schema_parameters <- function(arguments) {
  if (length(arguments) == 0) {
    return(list())
  }

  arg_names <- names2(arguments)
  arg_required <- map_lgl(arguments, function(arg) {
    arg@required %||% FALSE
  })

  properties <- lapply(arguments, function(arg) {
    list(
      type = arg@type,
      description = arg@description
    )
  })
  names(properties) <- arg_names

  list(
    type = "object",
    properties = properties,
    required = arg_names[arg_required]
  )
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
