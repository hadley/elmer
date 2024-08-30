#' @include coro-utils.R
NULL

#' @examples
#' chat <- new_chat()
#' chat$chat("What is the difference between a tibble and a data frame? Answer briefly")
#' chat$chat("Please summarise into a very concise bulleted list.", stream = FALSE)
#' chat$chat("Even more concise!!!", stream = FALSE)
#' chat$chat("Even more concise! Use emoji to save characters", stream = FALSE)
#'
#' chat <- new_chat()
#' chat$add_tool(rnorm)
#' chat$chat("Give me five numbers from a random normal distribution. Briefly explain your work.")
new_chat <- function(system_prompt = NULL,
                     base_url = "https://api.openai.com/v1",
                     api_key = open_ai_key(),
                     model = "gpt-4o-mini") {


  system_prompt <- system_prompt %||%
    "You are a helpful assistant from New Zealand who is an experienced R programmer"

  chat <- Chat$new(
    base_url = base_url,
    model = model,
    api_key = api_key
  )
  chat$add_message(list(
    role = "system",
    content = system_prompt
  ))
  chat
}

Chat <- R6::R6Class("Chat", public = list(
  base_url = NULL,
  model = NULL,
  api_key = NULL,

  messages = NULL,
  tools = NULL,

  initialize = function(base_url, model, api_key) {
    self$base_url <- base_url
    self$model <- model
    self$api_key <- api_key
  },

  add_message = function(message) {
    self$messages <- c(self$messages, list(message))
    invisible(self)
  },

  add_tool = function(tool) {
    self$tools <- c(self$tools, list(tool))
    invisible(self)
  },

  register_tool = function(name, description, arguments, strict = TRUE) {
    tool <- tool_def(
      name = name,
      description = description,
      arguments = arguments,
      strict = strict
    )
    self$add_tool(tool)
  },

  # Returns a single message (the final response from the assistant), even if
  # multiple rounds of back and forth happened.
  chat = function(text) {
    coro::collect(self$chat_impl(text, stream = FALSE))
    last_message <- self$messages[[length(self$messages)]]
    stopifnot(identical(last_message[["role"]], "assistant"))
    last_message$content
  },

  # Yields completion chunks.
  stream = function(text) {
    self$chat_impl(text, stream = TRUE)
  },

  # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
  # complete assistant messages.
  chat_impl = generator_method(function(self, text, stream) {
    self$add_message(list(role = "user", content = text))
    for (chunk in self$submit_messages(stream = stream)) {
      yield(chunk)
    }
    for (chunk in self$tool_loop(stream = stream)) {
      yield(chunk)
    }
    invisible(self)
  }),

  # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
  # complete assistant messages.
  submit_messages = generator_method(function(self, stream) {
    response <- open_ai_chat(
      messages = self$messages,
      tools = self$tools,
      base_url = self$base_url,
      model = self$model,
      stream = stream,
      api_key = self$api_key
    )
    
    if (stream) {
      result <- list()
      for (chunk in response) {
        result <- merge_dicts(result, chunk)
        yield(chunk$choices[[1]]$delta)
      }
      self$add_message(result$choices[[1]]$delta)
    } else {
      yield(response)
      self$add_message(response$choices[[1]]$message)
    }
    
    invisible(self)
  }),

  # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
  # complete assistant messages.
  tool_loop = generator_method(function(self, stream) {
    if (is.null(self$tools)) {
      return()
    }
  
    last_message <- self$messages[[length(self$messages)]]
    tool_message <- call_tools(last_message)
  
    if (is.null(tool_message)) {
      return()
    }
    self$messages <- c(self$messages, tool_message)
    for (chunk in self$submit_messages(stream = stream)) {
      yield(chunk)
    }
  })
))
