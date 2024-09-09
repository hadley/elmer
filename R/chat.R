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
#' chat <- new_chat_openai()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
#'
#' chat <- new_chat_openai()
#' chat$register_tool(
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
#' ")
new_chat_openai <- function(system_prompt = NULL,
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

  ChatOpenAI$new(
    base_url = base_url,
    model = model,
    api_key = api_key,
    system_prompt = system_prompt,
    quiet = quiet
  )
}

#' @rdname new_chat_openai
ChatOpenAI <- R6::R6Class("ChatOpenAI",
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

    #' @description Submit text to the chatbot, and return the response as a
    #'   simple string. If not in quiet mode, the response will be printed to
    #'   stdout as it is received.
    #' @param text The text to send to the chatbot.
    #' @param quiet Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `quiet` set when the chat object was created
    #'   will be used.
    #' @returns A string response (probably Markdown).
    chat = function(text, quiet = NULL) {
      check_string(text)

      quiet <- quiet %||% private$quiet

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(text, stream = !quiet, quiet = quiet))
      last_message <- private$messages[[length(private$messages)]]
      stopifnot(identical(last_message[["role"]], "assistant"))

      if (!quiet) {
        invisible(last_message$content)
      } else {
        last_message$content
      }
    },

    #' @description Submit text to the chatbot, and receive a promise that
    #'   resolves with the response all at once. If not in quiet mode, the
    #'   response will be printed to stdout as it is received.
    #' @param text The text to send to the chatbot.
    #' @param quiet Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `quiet` set when the chat object was created
    #'   will be used.
    #' @returns A promise that resolves to a string (probably Markdown).
    chat_async = function(text, quiet = NULL) {
      check_string(text)

      quiet <- quiet %||% private$quiet

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(private$chat_impl_async(text, stream = FALSE, quiet = quiet))
      promises::then(done, function(dummy) {
        last_message <- private$messages[[length(private$messages)]]
        stopifnot(identical(last_message[["role"]], "assistant"))
        last_message$content
      })
    },

    #' @description Submit text to the chatbot, returning streaming results.
    #' @param text The text to send to the chatbot.
    #' @param quiet Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `quiet` set when the chat object was created
    #'   will be used.
    #' @returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    stream = function(text, quiet = NULL) {
      private$chat_impl(text, stream = TRUE, quiet = quiet)
    },

    #' @description Submit text to the chatbot, returning asynchronously
    #'   streaming results.
    #' @param text The text to send to the chatbot.
    #' @param quiet Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `quiet` set when the chat object was created
    #'   will be used.
    #' @returns A [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html)
    #'   that yields string promises.
    stream_async = function(text, quiet = NULL) {
      private$chat_impl_async(text, stream = TRUE, quiet = quiet)
    },

    #' @description Enter an interactive chat console. This is a REPL-like
    #'   interface where you can chat with the assistant in real-time. (Only
    #'   available in [interactive()] mode.)
    console = function() {
      if (!interactive()) {
        abort("The chat console is only available in interactive mode.")
      }

      cli::cat_boxx(
        c("Entering chat console. Use \"\"\" for multi-line input.",
          "Press Ctrl+C to quit."),
        padding = c(0, 2, 0, 2),
        border_style = "double"
      )

      while (TRUE) {
        # Prompt for user input
        user_input <- readline(prompt = ">>> ")

        if (!grepl("\\S", user_input)) {
          next
        }

        if (grepl('^\\s*"""', user_input)) {
          while (TRUE) {
            next_input <- readline(prompt = '... ')
            user_input <- paste0(user_input, "\n", next_input)
            if (grepl('"""\\s*$', next_input)) {
              break
            }
          }
          # Strip leading and trailing """, using regex
          user_input <- gsub('^\\s*"""\\s*', '', user_input)
          user_input <- gsub('\\s*"""\\s*$', '', user_input)
        }

        # Process the input using the provided LLM function
        self$chat(user_input)
        cat("\n")
      }
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
    chat_impl = generator_method(function(self, private, text, stream, quiet = NULL) {
      private$add_message(list(role = "user", content = text))
      while (TRUE) {
        for (chunk in private$submit_messages(stream = stream, quiet = quiet)) {
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
    chat_impl_async = async_generator_method(function(self, private, text, stream, quiet = NULL) {
      private$add_message(list(role = "user", content = text))
      while (TRUE) {
        for (chunk in await_each(private$submit_messages_async(stream = stream, quiet = quiet))) {
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
    submit_messages = generator_method(function(self, private, stream, quiet = NULL) {
      quiet <- quiet %||% private$quiet

      response <- openai_chat(
        messages = private$messages,
        tools = private$tool_infos,
        base_url = private$base_url,
        model = private$model,
        stream = stream,
        api_key = private$api_key
      )

      if (!quiet) {
        # Like `cat()` but with automatic word wrapping
        emit <- cat_word_wrap()
      } else {
        emit <- function(...) invisible()
      }

      if (stream) {
        result <- list()
        any_content <- FALSE
        for (chunk in response) {
          result <- merge_dicts(result, chunk)
          if (!is.null(chunk$choices[[1]]$delta$content)) {
            emit(chunk$choices[[1]]$delta$content)
            yield(chunk$choices[[1]]$delta$content)
            any_content <- TRUE
          }
        }
        if (any_content) {
          emit("\n")
          yield("\n")
        }
        private$add_message(result$choices[[1]]$delta)
      } else {
        private$add_message(response$choices[[1]]$message)
        if (!is.null(response$choices[[1]]$message$content)) {
          emit(response$choices[[1]]$message$content)
          emit("\n")
          yield(response$choices[[1]]$message$content)
        }
      }

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant messages.
    submit_messages_async = async_generator_method(function(self, private, stream, quiet = NULL) {
      quiet <- quiet %||% private$quiet

      response <- openai_chat_async(
        messages = private$messages,
        tools = private$tool_infos,
        base_url = private$base_url,
        model = private$model,
        stream = stream,
        api_key = private$api_key
      )

      if (!quiet) {
        # Like `cat()` but with automatic word wrapping
        emit <- cat_word_wrap()
      } else {
        emit <- function(...) invisible()
      }

      if (stream) {
        result <- list()
        any_content <- FALSE
        for (chunk in await_each(response)) {
          result <- merge_dicts(result, chunk)
          if (!is.null(chunk$choices[[1]]$delta$content)) {
            emit(chunk$choices[[1]]$delta$content)
            yield(chunk$choices[[1]]$delta$content)
            any_content <- TRUE
          }
        }
        if (any_content) {
          emit("\n")
          yield("\n")
        }
        private$add_message(result$choices[[1]]$delta)
      } else {
        response_value <- await(response)
        private$add_message(response_value$choices[[1]]$message)
        if (!is.null(response_value$choices[[1]]$message$content)) {
          emit(response_value$choices[[1]]$message$content)
          emit("\n")
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
print.ChatOpenAI <- function(x, ...) {
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


last_message <- function(chat) {
  messages <- chat$.__enclos_env__$private$messages
  messages[[length(messages)]]
}
