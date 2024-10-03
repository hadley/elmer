#' @include utils-coro.R
NULL

#' Chat object
#'
#' @description
#' The Chat object represents a sequence of messages between the user and
#' chat API. You should generally not create this object yourself, but instead
#' call [new_chat_openai()] or friends.
#'
Chat <- R6::R6Class("Chat",
  public = list(
    #' @param provider A provider object.
    #' @param messages An unnamed list of messages to start the chat with (i.e.,
    #'   continuing a previous conversation). If `NULL` or zero-length list, the
    #'   conversation begins from scratch.
    #' @param seed Optional integer seed that ChatGPT uses to try and make output
    #'   more reproducible.
    #' @param echo If `TRUE`, the `chat()` method streams the response to stdout
    #'   (while also returning the final response). Note that this has no effect
    #'   on the `stream()`, `chat_async()`, and `stream_async()` methods.
    initialize = function(provider, messages, seed = NULL, echo = FALSE) {
      private$provider <- provider
      private$msgs <- messages %||% list()
      private$echo <- echo
    },

    #' @description The messages that have been sent and received so far
    #'   (optionally starting with the system prompt, if any).
    #' @param include_system_prompt Whether to include the system prompt in the
    #'   messages (if any exists).
    messages = function(include_system_prompt = FALSE) {
      if (length(private$msgs) == 0) {
        return(private$msgs)
      }

      if (!include_system_prompt && is_system_prompt(private$msgs[[1]])) {
        private$msgs[-1]
      } else {
        private$msgs
      }
    },

    #' @description The last message returned by the assistant.
    last_message = function() {
      private$msgs[[length(private$msgs)]]
    },

    #' @description Submit input to the chatbot, and return the response as a
    #'   simple string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images
    #'   (see [content_image_file()] and [content_image_url()].
    #' @param echo Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `echo` set when the chat object was created
    #'   will be used.
    chat = function(..., echo = NULL) {
      private$add_message(user_message(...))
      echo <- echo %||% private$echo

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(stream = echo, echo = echo))
      text <- format(self$last_message())

      if (echo) {
        invisible(text)
      } else {
        text
      }
    },

    #' @description Submit input to the chatbot, and receive a promise that
    #'   resolves with the response all at once.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @returns A promise that resolves to a string (probably Markdown).
    chat_async = function(...) {
      private$add_message(user_message(...))
      # content <- normalize_content(private$provider, ...)
      # input <- list(role = "user", content = content)

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(stream = FALSE, echo = FALSE)
      )
      promises::then(done, function(dummy) {
        text <- format(self$last_message())
        text
      })
    },

    #' @description Submit input to the chatbot, returning streaming results.
    #'   Returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream = function(...) {
      private$add_message(user_message(...))
      private$chat_impl(input, stream = TRUE, echo = FALSE)
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream_async = function(...) {
      private$add_message(user_message(...))
      private$chat_impl_async(input, stream = TRUE, echo = FALSE)
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   If the chatbot decides to use the function,  elmer will automatically
    #'   call it and submit the results back. (See [create_tool_metadata()] for
    #'   an AI-enabled helper function that can write a `register_tool` call
    #'   for you in some cases.)
    #' @param fun The function to be invoked when the tool is called.
    #' @param name The name of the function.
    #' @param description A detailed description of what the function does.
    #'   Generally, the more information that you can provide here, the better.
    #' @param arguments A named list of arguments that the function accepts.
    #'   Should be a named list of objects created by [tool_arg()].
    #' @param strict Should the argument definition be strictly enforced? If
    #'   `TRUE`, enables [Structured
    #'   Output](https://platform.openai.com/docs/guides/structured-outputs)
    #'   mode, which comes with a number of [additional
    #'   requirements](https://platform.openai.com/docs/guides/structured-outputs/supported-schemas).
    register_tool = function(fun, name = NULL, description, arguments = list(), strict = FALSE) {
      if (is.null(name)) {
        fun_expr <- enexpr(fun)
        if (is.name(fun_expr)) {
          name <- as.character(fun_expr)
        } else {
          name <- paste0("tool", length(private$tool_infos))
        }
      }

      check_function(fun)

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
  active = list(
    #' @field system_prompt The system prompt, if any, as a string.
    system_prompt = function() {
      if (length(private$msgs) == 0) {
        return(NULL)
      }
      if (private$msgs[[1]]@role != "system") {
        return(NULL)
      }
      private$msgs[[1]][["content"]]
    }
  ),
  private = list(
    provider = NULL,

    msgs = NULL,
    echo = NULL,

    # OpenAI-compliant tool metadata
    tool_infos = NULL,
    # Named list of R functions that implement tools
    tool_funs = NULL,

    add_message = function(message) {
      if (!inherits(message, chat_message)) {
        cli::cli_abort("Invalid input", .internal = TRUE)
      }

      private$msgs[[length(private$msgs) + 1]] <- message
      invisible(self)
    },

    add_user_contents = function(contents) {
      stopifnot(is.list(contents))
      if (length(contents) == 0) {
        return(invisible(self))
      }

      i <- length(private$msgs)

      if (private$msgs[[i]]@role != "user") {
        private$msgs[[i + 1]] <- chat_message("user", content = contents)
      } else {
        private$msgs[[i]]@content <- c(private$msgs[[i]]@content, contents)
      }
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
    chat_impl = generator_method(function(self, private, stream, echo) {
      repeat {
        for (chunk in private$submit_messages(stream = stream, echo = echo)) {
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
    chat_impl_async = async_generator_method(function(self, private, stream, echo) {
      repeat {
        for (chunk in await_each(private$submit_messages_async(stream = stream, echo = echo))) {
          yield(chunk)
        }
        tools_called <- await(private$invoke_tools_async())
        if (!tools_called) {
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
    submit_messages = generator_method(function(self, private, stream, echo) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "stream" else "value",
        messages = private$msgs,
        tools = private$tool_infos
      )
      emit <- emitter(echo)

      if (stream) {
        result <- list()
        any_text <- FALSE
        result <- NULL
        for (chunk in response) {
          text <- stream_text(private$provider, chunk)
          if (!is.null(text)) {
            emit(text)
            yield(text)
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }
        # Ensure messages always end in a newline
        if (any_text) {
          emit("\n")
          yield("\n")
        }

        message <- stream_message(private$provider, result)
      } else {
        text <- value_text(private$provider, response)
        if (!is.null(text)) {
          text <- paste0(text, "\n")
          emit(text)
          yield(text)
        }
        message <- value_message(private$provider, response)
      }
      private$add_message(message)

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant messages.
    submit_messages_async = async_generator_method(function(self, private, stream, echo) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "async-stream" else "async-value",
        messages = private$msgs,
        tools = private$tool_infos
      )
      emit <- emitter(echo)

      if (stream) {
        any_text <- FALSE
        result <- NULL
        for (chunk in await_each(response)) {
          text <- stream_text(private$provider, chunk)
          if (!is.null(text)) {
            emit(text)
            yield(text)
            any_text <- TRUE
          }

          result <- stream_merge_chunks(private$provider, result, chunk)
        }
        # Ensure messages always end in a newline
        if (any_text) {
          emit("\n")
          yield("\n")
        }

        message <- stream_message(private$provider, result)
      } else {
        result <- await(response)

        text <- value_text(private$provider, result)
        if (!is.null(text)) {
          text <- paste0(text, "\n")
          emit(text)
          yield(text)
        }
        message <- value_message(private$provider, result)
      }
      private$add_message(message)

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    invoke_tools = function() {
      if (length(private$tool_infos) == 0) {
        return(FALSE)
      }

      tool_results <- invoke_tools(self$last_message(), private$tool_funs)
      private$add_user_contents(tool_results)

      length(tool_results) > 0
    },

    invoke_tools_async = async_method(function(self, private) {
      if (length(private$tool_infos) == 0) {
        return(FALSE)
      }

      tool_results <- await(invoke_tools_async(self$last_message(), private$tool_funs))
      private$add_user_contents(tool_results)

      length(tool_results) > 0
    })
  )
)

#' @export
print.Chat <- function(x, ...) {
  msgs <- x$messages(include_system_prompt = TRUE)
  cat(paste0("<Chat messages=", length(msgs), ">\n"))
  for (message in msgs) {
    color <- switch(message@role,
      user = cli::col_blue,
      assistant = cli::col_green,
      system = cli::col_br_white,
      identity
    )
    cli::cat_rule(cli::format_inline("{color(message@role)}"))
    for (content in message@content) {
      cat_line(format(content))
    }
  }

  invisible(x)
}
