#' Chat with a model hosted on perplexity.ai
#'
#' @description
#' Sign up at <https://www.perplexity.ai>.
#'
#' Perplexity AI is a platform for running LLMs that are capable of
#' searching the web in real-time to help them answer questions with
#' information that may not have been available when the model was
#' trained.
#'
#' This function is a lightweight wrapper around [chat_openai()] with
#' the defaults tweaked for Perplexity AI.
#'
#' @export
#' @family chatbots
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `PERPLEXITY_API_KEY` environment
#'   variable.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @examples
#' \dontrun{
#' chat <- chat_perplexity()
#' chat$chat("Tell me three jokes about statisticians")
#' }
chat_perplexity <- function(system_prompt = NULL,
                            turns = NULL,
                            base_url = "https://api.perplexity.ai/",
                            api_key = perplexity_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = NULL) {

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
