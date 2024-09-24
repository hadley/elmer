#' Chat with models in the GitHub model marketplace
#'
#' @description
#' This assumes that you have been accepted into the beta access program
#' at <https://github.com/marketplace/models>.
#'
#' This function is a lightweight wrapper around [new_chat_openai()] with
#' the defaults tweaked for the GitHub model marketplace.
#'
#' @export
#' @inheritParams new_chat_openai
new_chat_github <- function(system_prompt = NULL,
                            messages = NULL,
                            base_url = "https://models.inference.ai.azure.com/",
                            api_key = github_key(),
                            model = NULL,
                            seed = NULL,
                            api_args = list(),
                            echo = FALSE) {

  check_installed("gitcreds")

  new_chat_openai(
    system_prompt = system_prompt,
    messages = messages,
    base_url = base_url,
    api_key = api_key,
    model = model,
    seed = seed,
    api_args = api_args,
    echo = echo
  )
}

github_key <- function() {
  withCallingHandlers(
    gitcreds::gitcreds_get()$password,
    errror = function(cnd) {
      cli::cli_abort("Failed to find GITHUB_PAT.", parent = cnd)
    }
  )
}
