#' Chat with models from perplexity.ai
#'
#' @description
#' Sign up at <https://www.perplexity.ai>.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for the groq.
#'
#' @export
#' @family chatbots
#' @inheritParams chat_openai
chat_perplexity <- function(system_prompt = NULL,
                                turns = NULL,
                                base_url = "https://api.perplexity.ai/",
                                api_key = perplexity_key(),
                                model = NULL,
                                seed = NULL,
                                api_args = list(),
                                echo = FALSE) {

  model <- set_default(model, "llama-3.1-sonar-small-128k-online")

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

perplexity_key <- function() {
  key_get("PERPLEXITY_API_KEY")
}
