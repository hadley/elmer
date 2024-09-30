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

      if (!include_system_prompt && private$msgs[[1]][["role"]] == "system") {
        private$msgs[-1]
      } else {
        private$msgs
      }
    },

    #' @description Submit input to the chatbot, and return the response as a
    #'   simple string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images
    #'   (see [content_image_file()] and [content_image_url()].
    #' @param echo Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `echo` set when the chat object was created
    #'   will be used.
    chat = function(..., echo = NULL) {
      input <- normalize_chat_input(...)

      echo <- echo %||% private$echo

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(input, stream = echo, echo = echo))
      last_message <- private$msgs[[length(private$msgs)]]
      stopifnot(identical(last_message[["role"]], "assistant"))

      if (echo) {
        invisible(last_message$content)
      } else {
        last_message$content
      }
    },

    #' @description Submit input to the chatbot, and receive a promise that
    #'   resolves with the response all at once.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    #' @returns A promise that resolves to a string (probably Markdown).
    chat_async = function(...) {
      input <- normalize_chat_input(...)

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(input, stream = FALSE, echo = FALSE)
      )
      promises::then(done, function(dummy) {
        last_message <- private$msgs[[length(private$msgs)]]
        stopifnot(identical(last_message[["role"]], "assistant"))
        last_message$content
      })
    },

    #' @description Submit input to the chatbot, returning streaming results.
    #'   Returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream = function(...) {
      input <- normalize_chat_input(...)
      private$chat_impl(input, stream = TRUE, echo = FALSE)
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream_async = function(...) {
      input <- normalize_chat_input(...)
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
      if (private$msgs[[1]][["role"]] != "system") {
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
      private$msgs <- c(private$msgs, list(message))
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
    chat_impl = generator_method(function(self, private, user_message, stream, echo) {
      private$add_message(user_message)
      while (TRUE) {
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
    chat_impl_async = async_generator_method(function(self, private, user_message, stream, echo) {
      private$add_message(user_message)
      while (TRUE) {
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

      last_message <- private$msgs[[length(private$msgs)]]
      tool_calls <- value_tool_calls(private$provider, last_message, private$tool_funs)
      tool_messages <- call_tools(private$provider, tool_calls)

      if (length(tool_messages) > 0) {
        private$msgs <- c(private$msgs, tool_messages)
        TRUE
      } else {
        FALSE
      }
    },

    invoke_tools_async = async_method(function(self, private) {
      if (length(private$tool_infos) == 0) {
        return(FALSE)
      }

      last_message <- private$msgs[[length(private$msgs)]]
      tool_calls <- value_tool_calls(private$provider, last_message, private$tool_funs)
      tool_messages <- await(call_tools_async(private$provider, tool_calls))

      if (length(tool_messages) > 0) {
        private$msgs <- c(private$msgs, tool_messages)
        TRUE
      } else {
        FALSE
      }
    })
  )
)

#' @export
print.Chat <- function(x, ...) {
  msgs <- x$messages(include_system_prompt = TRUE)
  msgs_without_system_prompt <- x$messages(include_system_prompt = FALSE)
  cat(paste0("<Chat messages=", length(msgs_without_system_prompt), ">\n"))
  for (message in msgs) {
    color <- switch(message$role,
      user = cli::col_blue,
      assistant = cli::col_green,
      identity
    )
    cli::cli_rule("{color(message$role)}")
    if (!is.null(message$content)) {
      # Using cli_text for word wrapping. Passing `"{message$content}"` instead of
      # `message$content` to avoid evaluation of the (potentially malicious)
      # content.
      cli::cli_text("{format_content(message$content)}")
    }
    if (!is.null(message$tool_calls)) {
      cli::cli_text("Tool calls:")
      for (tool_call in message$tool_calls) {
        funcname <- tool_call$`function`$name
        args <- tool_call$`function`$arguments
        tryCatch({
          args_parsed <- jsonlite::parse_json(tool_call$`function`$arguments)
          args <- call2(funcname, !!!args_parsed)
          cli::cli_text(format(args))
        }, error = function(e) {
          # In case parsing the JSON fails
          cli::cli_text("{funcname}({args})")
        })
      }
    }
  }

  invisible(x)
}


format_content <- function(content) {
  if (is.character(content)) {
    content
  } else if (is.list(content)) {
    paste0(lapply(content, function(x) {
      type <- x[["type"]]
      value <- x[[type]]
      paste0("[", type, "]: ", value)
    }), collapse = "\n")
  }
}

last_message <- function(chat) {
  messages <- chat$messages()
  messages[[length(messages)]]
}
