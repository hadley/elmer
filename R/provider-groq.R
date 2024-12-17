#' @include provider-openai.R
NULL

#' Chat with a model hosted on Groq
#'
#' @description
#' Sign up at <https://groq.com>.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for groq.
#'
#' @export
#' @family chatbots
#' @inheritParams chat_openai
chat_groq <- function(system_prompt = NULL,
                          turns = NULL,
                          base_url = "https://api.groq.com/openai/v1",
                          api_key = groq_key(),
                          model = NULL,
                          seed = NULL,
                          api_args = list(),
                          echo = NULL) {

  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "llama3-8b-8192")
  echo <- check_echo(echo)

  if (is_testing() && is.null(seed)) {
    seed <- seed %||% 1014
  }

  provider <- ProviderGroq(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderGroq <- new_class("ProviderGroq", parent = ProviderOpenAI)


# method(value_turn, ProviderGroq) <- function(provider, result, has_type = FALSE) {
#   if (has_name(result$choices[[1]], "delta")) { # streaming
#     message <- result$choices[[1]]$delta
#   } else {
#     message <- result$choices[[1]]$message
#   }

#   if (has_type) {
#     json <- jsonlite::parse_json(message$content[[1]])
#     content <- list(ContentJson(json))
#   } else {
#     content <- lapply(message$content, as_content)
#   }

#   if (has_name(message, "tool_calls")) {
#     calls <- lapply(message$tool_calls, function(call) {
#       name <- call$`function`$name
#       # TODO: record parsing error
#       args <- jsonlite::parse_json(call$`function`$arguments)
#       ContentToolRequest(name = name, arguments = args, id = call$id)
#     })
#     content <- c(content, calls)
#   }
#   tokens <- c(
#     result$usage$prompt_tokens %||% NA_integer_,
#     result$usage$completion_tokens %||% NA_integer_
#   )
#   tokens_log(paste0("OpenAI-", provider@base_url), tokens)

#   Turn(message$role, content, json = result, tokens = tokens)
# }


method(as_json, list(ProviderGroq, Turn)) <- function(provider, x) {
  if (x@role == "assistant") {
    # Tool requests come out of content and go into own argument
    is_tool <- map_lgl(x@contents, S7_inherits, ContentToolRequest)
    tool_calls <- as_json(provider, x@contents[is_tool])

    # Grok contents is just a string. Hopefully it never sends back more
    # than a single text response.
    content <- x@contents[!is_tool][[1]]@text

    list(
      compact(list(role = "assistant", content = content, tool_calls = tool_calls))
    )
  } else {
    as_json(super(provider, ProviderOpenAI), x)
  }
}

# method(as_json, list(ProviderGroq, TypeObject)) <- function(provider, x) {
#   if (x@additional_properties) {
#     cli::cli_abort("{.arg .additional_properties} not supported for Groq.")
#   }
#   required <- map_lgl(x@properties, function(prop) prop@required)

#   compact(list(
#     type = "object",
#     description = x@description,
#     properties = as_json(provider, x@properties),
#     required = as.list(names2(x@properties)[required])
#   ))
# }


# method(as_json, list(ProviderGroq, ToolDef)) <- function(provider, x) {
#   list(
#     type = "function",
#     "function" = compact(list(
#       name = x@name,
#       description = x@description,
#       parameters = as_json(provider, x@arguments)
#     ))
#   )
# }

groq_key <- function() {
  key_get("GROQ_API_KEY")
}
