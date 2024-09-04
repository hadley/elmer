#' @include coro-utils.R
NULL

#' @examples
#' chat <- new_chat_async()
#' chat$chat("What is the difference between a tibble and a data frame? Answer briefly")
#' chat$chat("Please summarise into a very concise bulleted list.", stream = FALSE)
#' chat$chat("Even more concise!!!", stream = FALSE)
#' chat$chat("Even more concise! Use emoji to save characters", stream = FALSE)
#'
#' chat <- new_chat()
#' chat$add_tool(rnorm)
#' chat$chat("Give me five numbers from a random normal distribution. Briefly explain your work.")
#' @export
new_chat_async <- function(system_prompt = NULL,
                     base_url = "https://api.openai.com/v1",
                     api_key = open_ai_key(),
                     model = "gpt-4o-mini") {


  system_prompt <- system_prompt %||%
    "You are a helpful assistant from New Zealand who is an experienced R programmer"

  chat <- AsyncChat$new(
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

AsyncChat <- R6::R6Class("AsyncChat", public = list(
  base_url = NULL,
  model = NULL,
  api_key = NULL,

  messages = NULL,
  # OpenAI-compliant tool metadata
  tool_infos = NULL,
  # Named list of R functions that implement tools
  tool_funs = NULL,

  initialize = function(base_url, model, api_key) {
    self$base_url <- base_url
    self$model <- model
    self$api_key <- api_key
  },

  add_message = function(message) {
    self$messages <- c(self$messages, list(message))
    invisible(self)
  },

  add_tool = function(name, fun, tool) {
    # Remove existing, if any
    if (!is.null(self$tool_funs[[name]])) {
      self$tool_infos <- Filter(function(info) info$name != name, self$tool_infos)
    }

    self$tool_infos <- c(self$tool_infos, list(tool))
    self$tool_funs[[name]] <- fun

    invisible(self)
  },

  register_tool = function(fun, name, description, arguments, strict = TRUE) {
    if (!is.function(fun)) {
      stop("fun must be a function")
    }
    tool <- tool_def(
      name = name,
      description = description,
      arguments = arguments,
      strict = strict
    )
    self$add_tool(name, fun, tool)
  },

  # Returns a single message (the final response from the assistant), even if
  # multiple rounds of back and forth happened.
  chat = function(text) {
    done <- coro::async_collect(self$chat_impl(text, stream = FALSE))
    promises::then(done, function(dummy) {
      last_message <- self$messages[[length(self$messages)]]
      stopifnot(identical(last_message[["role"]], "assistant"))
      last_message$content
    })
  },

  # Yields completion chunks.
  stream = function(text) {
    self$chat_impl(text, stream = TRUE)
  },

  # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
  # complete assistant messages.
  chat_impl = async_generator_method(function(self, text, stream) {
    self$add_message(list(role = "user", content = text))
    while (TRUE) {
      for (chunk in await_each(self$submit_messages(stream = stream))) {
        yield(chunk)
      }
      if (!self$invoke_tools()) {
        break
      }
    }

    # Work around https://github.com/r-lib/coro/issues/51
    if (FALSE) {
      yield(NULL)
    }
  }),

  # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
  # complete assistant messages.
  submit_messages = async_generator_method(function(self, stream) {
    response <- open_ai_chat_async(
      messages = self$messages,
      tools = self$tool_infos,
      base_url = self$base_url,
      model = self$model,
      stream = stream,
      api_key = self$api_key
    )

    if (stream) {
      result <- list()
      for (chunk in await_each(response)) {
        result <- merge_dicts(result, chunk)
        if (!is.null(chunk$choices[[1]]$delta$content)) {
          yield(chunk$choices[[1]]$delta$content)
        }
      }
      self$add_message(result$choices[[1]]$delta)
    } else {
      response_value <- await(response)
      self$add_message(response_value$choices[[1]]$message)
      yield(response_value)
    }

    # Work around https://github.com/r-lib/coro/issues/51
    if (FALSE) {
      yield(NULL)
    }
  }),

  invoke_tools = function() {
    if (length(self$tool_infos) > 0) {
      last_message <- self$messages[[length(self$messages)]]
      tool_message <- call_tools(self$tool_funs, last_message)

      if (!is.null(tool_message)) {
        self$messages <- c(self$messages, tool_message)
        return(TRUE)
      }
    }
    FALSE
  }
))
