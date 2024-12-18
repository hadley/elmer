#' @include provider-openai.R
#' @include content.R
NULL

# https://learn.microsoft.com/en-us/azure/ai-services/openai/reference#chat-completions

#' Chat with a model hosted on Azure OpenAI
#'
#' @description
#' The [Azure OpenAI server](https://azure.microsoft.com/en-us/products/ai-services/openai-service)
#' hosts a number of open source models as well as proprietary models
#' from OpenAI.
#'
#' @param endpoint Azure OpenAI endpoint url with protocol and hostname, i.e.
#'  `https://{your-resource-name}.openai.azure.com`. Defaults to using the
#'   value of the `AZURE_OPENAI_ENDPOINT` envinronment variable.
#' @param deployment_id Deployment id for the model you want to use.
#' @param api_version The API version to use.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `AZURE_OPENAI_API_KEY` environment
#'   variable.
#' @param token Azure token for authentication. This is typically not required for
#' Azure OpenAI API calls, but can be used if your setup requires it.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_azure(deployment_id = "gpt-4o-mini")
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_azure <- function(endpoint = azure_endpoint(),
                       deployment_id,
                       api_version = NULL,
                       system_prompt = NULL,
                       turns = NULL,
                       api_key = azure_key(),
                       token = NULL,
                       api_args = list(),
                       echo = c("none", "text", "all")) {
  check_string(endpoint)
  check_string(deployment_id)
  api_version <- set_default(api_version, "2024-06-01")
  turns <- normalize_turns(turns, system_prompt)
  echo <- check_echo(echo)

  base_url <- paste0(endpoint, "/openai/deployments/", deployment_id)

  provider <- ProviderAzure(
    base_url = base_url,
    endpoint = endpoint,
    model = deployment_id,
    api_version = api_version,
    token = token,
    extra_args = api_args,
    api_key = api_key
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderAzure <- new_class(
  "ProviderAzure",
  parent = ProviderOpenAI,
  properties = list(
    api_key = prop_string(),
    token = prop_string(allow_null = TRUE),
    endpoint = prop_string(),
    api_version = prop_string()
  )
)

# https://learn.microsoft.com/en-us/azure/ai-services/openai/how-to/switching-endpoints#api-key
azure_key <- function() {
  key_get("AZURE_OPENAI_API_KEY")
}

azure_endpoint <- function() {
  key_get("AZURE_OPENAI_ENDPOINT")
}

# https://learn.microsoft.com/en-us/azure/ai-services/openai/reference#chat-completions
method(chat_request, ProviderAzure) <- function(provider,
                                                stream = TRUE,
                                                turns = list(),
                                                tools = list(),
                                                type = NULL,
                                                extra_args = list()) {

  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_url_query(req, `api-version` = provider@api_version)
  req <- req_headers(req, `api-key` = provider@api_key, .redact = "api-key")
  if (!is.null(provider@token)) {
    req <- req_auth_bearer_token(req, provider@token)
  }
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) resp_body_json(resp)$message)

  messages <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  if (!is.null(type)) {
    response_format <- list(
      type = "json_schema",
      json_schema = list(
        name = "structured_data",
        schema = as_json(provider, type),
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
