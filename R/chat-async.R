#' @include coro-utils.R
NULL

#' Create a chatbot that speaks to an OpenAI compatible endpoint
#'
#' This function returns an R6 object that takes care of managing the state
#' associated with the chat; i.e. it records the messages that you send to the
#' server, and the messages that you receive back. If you register a tool
#' (aka an R function), it also takes care of the tool loop.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param base_url The base URL to the endpoint; the default uses ChatGPT.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
#'   variable.
#' @param model The model to use for the chat; defaults to GPT-4o mini.
#' @param quiet If `TRUE` does not print output as its received.
#' @export
#' @examplesIf elmer:::openai_key_exists()
#' library(promises)
#'
#' chat <- new_chat_async_openai()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ") %...>% cat()
#'
#' chat <- new_chat_async_openai()
#' chat$register_tool(
#'   fun = rnorm,
#'   name = "rnorm",
#'   description = "Drawn numbers from a random normal distribution",
#'   arguments = list(
#'     n = tool_arg(
#'       type = "integer",
#'       description = "The number of observations. Must be a positive integer."
#'     ),
#'     mean = tool_arg(
#'       type = "number",
#'       description = "The mean value of the distribution."
#'     ),
#'     sd = tool_arg(
#'       type = "number",
#'       description = "The standard deviation of the distribution. Must be a non-negative number."
#'     )
#'   )
#' )
#' chat$chat("
#'   Give me five numbers from a random normal distribution.
#'   Briefly explain your work.
#' ") %...>% cat()
new_chat_async_openai <- function(system_prompt = NULL,
                                  base_url = "https://api.openai.com/v1",
                                  api_key = openai_key(),
                                  model = "gpt-4o-mini",
                                  quiet = FALSE) {
  check_string(system_prompt, allow_null = TRUE)
  check_string(base_url)
  check_string(api_key)
  check_string(model)
  check_bool(quiet)

  system_prompt <- system_prompt %||%
    "You are a helpful assistant from New Zealand who is an experienced R programmer"

  ChatAsyncOpenAI$new(
    base_url = base_url,
    model = model,
    api_key = api_key,
    system_prompt = system_prompt,
    quiet = quiet
  )
}

#' @rdname new_chat_async_openai
ChatAsyncOpenAI <- R6::R6Class("ChatOpenAI",
  public = list(
    #' @param system_prompt A system prompt to set the behavior of the assistant.
    #' @param base_url The base URL to the endpoint; the default uses ChatGPT.
    #' @param api_key The API key to use for authentication. You generally should
    #'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
    #'   variable.
    #' @param model The model to use for the chat; defaults to GPT-4o mini.
    #' @param quiet If `TRUE` does not print output as its received.
    initialize = function(base_url, model, api_key, system_prompt, quiet = TRUE) {
      private$base_url <- base_url
      private$model <- model
      private$api_key <- api_key
      private$quiet <- quiet

      private$add_message(list(
        role = "system",
        content = system_prompt
      ))
    },

    #' @description Submit text to the chatbot, and receive a promise that
    #'   resolves with the response all at once.
    #' @param text The text to send to the chatbot.
    #' @returns A promise that resolves to a string (probably Markdown).
    chat = function(text) {
      check_string(text)

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(private$chat_impl(text, stream = FALSE))
      promises::then(done, function(dummy) {
        last_message <- private$messages[[length(private$messages)]]
        stopifnot(identical(last_message[["role"]], "assistant"))
        last_message$content
      })
    },

    #' @description Submit text to the chatbot, returning asynchronously
    #'   streaming results.
    #' @param text The text to send to the chatbot.
    #' @returns A [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html)
    #'   that yields string promises.
    stream = function(text) {
      private$chat_impl(text, stream = TRUE)
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   If the chatbot decides to use the function, elmer will automatically
    #'   call it and submit the results back.
    #' @param fun The function to be invoked when the tool is called.
    #' @param name The name of the function.
    #' @param description A detailed description of what the function does.
    #'   Generally, the more information that you can provide here, the better.
    #' @param arguments A named list of arguments that the function accepts.
    #'   Should be a named list of objects created by [tool_arg()].
    #' @param strict Should the argument definition be strictly enforced?
    register_tool = function(fun, name, description, arguments, strict = TRUE) {
      check_function(fun)
      check_string(name)
      check_string(description)
      check_bool(strict)

      tool <- tool_def(
        name = name,
        description = description,
        arguments = arguments,
        strict = strict
      )
      private$add_tool(name, fun, tool)
      invisible(self)
    }
  ),
  private = list(
    base_url = NULL,
    model = NULL,
    api_key = NULL,

    messages = NULL,
    quiet = NULL,

    # OpenAI-compliant tool metadata
    tool_infos = NULL,
    # Named list of R functions that implement tools
    tool_funs = NULL,

    add_message = function(message) {
      private$messages <- c(private$messages, list(message))
      invisible(self)
    },

    add_tool = function(name, fun, tool) {
      # Remove existing, if any
      if (!is.null(private$tool_funs[[name]])) {
        private$tool_infos <- Filter(function(info) info$name != name, private$tool_infos)
      }

      private$tool_infos <- c(private$tool_infos, list(tool))
      private$tool_funs[[name]] <- fun

      invisible(self)
    },

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant messages.
    chat_impl = async_generator_method(function(self, private, text, stream) {
      private$add_message(list(role = "user", content = text))
      while (TRUE) {
        for (chunk in await_each(private$submit_messages(stream = stream))) {
          yield(chunk)
        }
        if (!private$invoke_tools()) {
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
    submit_messages = async_generator_method(function(self, private, stream) {
      response <- openai_chat_async(
        messages = private$messages,
        tools = private$tool_infos,
        base_url = private$base_url,
        model = private$model,
        stream = stream,
        api_key = private$api_key
      )

      if (stream) {
        result <- list()
        for (chunk in await_each(response)) {
          result <- merge_dicts(result, chunk)
          if (!is.null(chunk$choices[[1]]$delta$content)) {
            if (!private$quiet) {
              cat(chunk$choices[[1]]$delta$content)
            }
            yield(chunk$choices[[1]]$delta$content)
          }
        }
        private$add_message(result$choices[[1]]$delta)
      } else {
        response_value <- await(response)
        private$add_message(response_value$choices[[1]]$message)
        if (!is.null(response_value$choices[[1]]$message$content)) {
          if (!private$quiet) {
            cat(response_value$choices[[1]]$message$content)
            cat("\n")
          }
          yield(response_value$choices[[1]]$message$content)
        }
      }

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    invoke_tools = function() {
      if (length(private$tool_infos) > 0) {
        last_message <- private$messages[[length(private$messages)]]
        tool_message <- call_tools(private$tool_funs, last_message)

        if (!is.null(tool_message)) {
          private$messages <- c(private$messages, tool_message)
          return(TRUE)
        }
      }
      FALSE
    }
  )
)

#' @export
print.ChatAsyncOpenAI <- function(x, ...) {
  cat("<ChatOpenAI>\n")
  messages <- x$.__enclos_env__$private$messages
  for (message in messages) {
    color <- switch(message$role,
      user = "blue",
      system = ,
      assistant = "green"
    )
    cat(cli::rule(message$role, col = color), "\n", sep = "")
    cat(message$content, "\n", sep = "")
  }

  invisible(x)
}
