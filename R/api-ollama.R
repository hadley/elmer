#' Connect to a local ollama instances
#'
#' @description
#' Download and install [ollama](https://ollama.com) and then you can
#' chat with it from R with `new_chat_ollama()`. To install additional
#' models, use the `ollama` command line, e.g. `ollama pull llama3.1`
#' or `ollama pull gemma2`.
#'
#' This function is a lightweight wrapper around [new_chat_openai()] with
#' the defaults tweaked for ollama.
#'
#' @inheritParams new_chat_openai
#' @export
new_chat_ollama <- function(system_prompt = NULL,
                            messages = NULL,
                            base_url = "http://localhost:11434/v1",
                            model,
                            seed = NULL,
                            api_args = list(),
                            echo = FALSE) {
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

  new_chat_openai(
    system_prompt = system_prompt,
    messages = messages,
    base_url = base_url,
    api_key = "ollama", # ignore
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
