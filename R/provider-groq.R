#' Chat with a model hosted on Groq
#'
#' @description
#' Sign up at <https://groq.com>.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for the groq.
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
  model <- set_default(model, "llama3-8b-8192")

  chat_openai(
    system_prompt = system_prompt,
    turns = turns,
    base_url = base_url,
    api_key = api_key,
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
}

groq_key <- function() {
  key_get("GROQ_API_KEY")
}
