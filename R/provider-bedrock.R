chat_bedrock <- function(system_prompt = NULL,
                         turns = NULL,
                         model = NULL,
                         profile = NULL,
                         echo = NULL) {
  turns <- normalize_turns(turns, system_prompt)
  model <- set_default(model, "anthropic.claude-3-5-sonnet-20240620-v1:0")
  echo <- check_echo(echo)

  provider <- ProviderBedrock(
    base_url = "",
    model = model,
    profile = profile
  )

  Chat$new(provider = provider, turns = turns, echo = FALSE)
}

ProviderBedrock <- new_class(
  "ProviderBedrock",
  parent = Provider,
  package = "elmer",
  properties = list(
    model = prop_string(),
    profile = prop_string(allow_null = TRUE)
  )
)



method(chat_request, ProviderBedrock) <- function(provider,
                                                  stream = TRUE,
                                                  turns = list(),
                                                  tools = list(),
                                                  extra_args = list()) {

  bedrock <- paws::bedrockruntime(credentials = list(profile = provider@profile))

  if (length(turns) >= 1 && is_system_prompt(turns[[1]])) {
    system <- turns[[1]]@text
  } else {
    system <- NULL
  }

  messages <- bedrock_messages(turns)
  tools <- unname(lapply(tools, bedrock_tool))

  bedrock$converse(
    model_id = provider@model,
    system = system,
    messages = messages,
    toolConfig = tools
  )
}
