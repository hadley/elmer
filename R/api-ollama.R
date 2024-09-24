#' Connect to a local ollama instances
#'
#' Download and install [ollama](https://ollama.com) and then you can
#' chat with it from R with `new_chat_ollama()`. To install additional
#' models, use the `ollama` command line, e.g. `ollama pull llama3.1`
#' or `ollama pull gemma2`.
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
  check_string(system_prompt, allow_null = TRUE)
  openai_check_conversation(messages, allow_null = TRUE)
  check_bool(echo)

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

  model <- new_openai_model(
    base_url = base_url,
    model = model,
    seed = seed,
    extra_args = api_args,
    api_key = "ollama" # ignored
  )

  messages <- openai_apply_system_prompt(system_prompt, messages)
  Chat$new(model = model, messages = messages, echo = echo)
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
