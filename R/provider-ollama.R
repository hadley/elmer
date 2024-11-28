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
#' @family chatbots
#' @export
chat_ollama <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "http://localhost:11434/v1",
                            model,
                            seed = NULL,
                            api_args = list(),
                            echo = NULL) {
  if (!has_ollama()) {
    cli::cli_abort("Can't find locally running ollama.")
  }

  if (missing(model)) {
    models <- ollama_models()
    cli::cli_abort(c(
      "Must specify {.arg model}.",
      i = "Locally installed models: {.str {models}}."
    ))
  }

  chat_openai(
    system_prompt = system_prompt,
    turns = turns,
    base_url = base_url,
    api_key = "ollama", # ignored
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
}

ollama_models <- function() {
  req <- request("http://localhost:11434/api/tags")
  resp <- req_perform(req)
  json <- resp_body_json(resp)

  names <- map_chr(json$models, "[[", "name")
  gsub(":latest$", "", names)
}

has_ollama <- function() {
  tryCatch(
    {
      req_perform(request("http://localhost:11434/api/tags"))
      TRUE
    },
    httr2_error = function(cnd) FALSE
  )
}
