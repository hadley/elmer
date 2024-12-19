#' Chat with a local Ollama model
#'
#' @description
#' To use `chat_ollama()` first download and install
#' [Ollama](https://ollama.com). Then install some models from the command line,
#' e.g. with `ollama pull llama3.1` or `ollama pull gemma2`.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for ollama.
#'
#' ## Known limitations
#'
#' * Tool calling is not supported with streaming (i.e. when `echo` is
#'   `"text"` or `"all"`)
#' * Tool calling generally seems quite weak, at least with the models I have
#'   tried it with.
#'
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @export
#' @examples
#' \dontrun{
#' chat <- chat_ollama(model = "llama3.2")
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_ollama <- function(system_prompt = NULL,
                        turns = NULL,
                        base_url = "http://localhost:11434",
                        model,
                        seed = NULL,
                        api_args = list(),
                        echo = NULL) {
  if (!has_ollama(base_url)) {
    cli::cli_abort("Can't find locally running ollama.")
  }

  if (missing(model)) {
    models <- ollama_models(base_url)
    cli::cli_abort(c(
      "Must specify {.arg model}.",
      i = "Locally installed models: {.str {models}}."
    ))
  }
  echo <- check_echo(echo)

  chat_openai(
    system_prompt = system_prompt,
    turns = turns,
    base_url = file.path(base_url, "v1"), ## the v1 portion of the path is added for openAI compatible API
    api_key = "ollama", # ignored
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
}

ollama_models <- function(base_url = "http://localhost:11434") {
  req <- request(base_url)
  req <- req_url_path(req, "api/tags")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  names <- map_chr(json$models, "[[", "name")
  gsub(":latest$", "", names)
}

has_ollama <- function(base_url = "http://localhost:11434") {
  tryCatch(
    {
      req <- request(base_url)
      req <- req_url_path(req, "api/tags")
      req_perform(req)
      TRUE
    },
    httr2_error = function(cnd) FALSE
  )
}
