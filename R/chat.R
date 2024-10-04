#' @include utils-coro.R
NULL

#' Chat object
#'
#' @description
#' The Chat object represents a sequence of turns between the user and
#' assistant (chat API). You should generally not create this object yourself,
#' but instead call [new_chat_openai()] or friends.
#'
Chat <- R6::R6Class("Chat",
  public = list(
    #' @param provider A provider object.
    #' @param turns An unnamed list of turns to start the chat with (i.e.,
    #'   continuing a previous conversation). If `NULL` or zero-length list, the
    #'   conversation begins from scratch.
    #' @param seed Optional integer seed that ChatGPT uses to try and make output
    #'   more reproducible.
    #' @param echo If `TRUE`, the `chat()` method streams the response to stdout
    #'   (while also returning the final response). Note that this has no effect
    #'   on the `stream()`, `chat_async()`, and `stream_async()` methods.
    initialize = function(provider, turns, seed = NULL, echo = FALSE) {
      private$provider <- provider
      private$.turns <- turns %||% list()
      private$echo <- echo
    },

    #' @description The turns that have been sent and received so far
    #'   (optionally starting with the system prompt, if any).
    #' @param include_system_prompt Whether to include the system prompt in the
    #'   turns (if any exists).
    turns = function(include_system_prompt = FALSE) {
      if (length(private$.turns) == 0) {
        return(private$.turns)
      }

      if (!include_system_prompt && is_system_prompt(private$.turns[[1]])) {
        private$.turns[-1]
      } else {
        private$.turns
      }
    },

    #' @description The last turn returned by the assistant.
    last_turn = function() {
      private$.turns[[length(private$.turns)]]
    },

    #' @description Submit input to the chatbot, and return the response as a
    #'   simple string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images
    #'   (see [content_image_file()] and [content_image_url()].
    #' @param echo Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `echo` set when the chat object was created
    #'   will be used.
    chat = function(..., echo = NULL) {
      turn <- user_turn(...)
      echo <- echo %||% private$echo

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(turn, stream = echo, echo = echo))

      text <- self$last_turn()@text

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
      turn <- user_turn(...)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(turn, stream = FALSE, echo = FALSE)
      )
      promises::then(done, function(dummy) {
        self$last_turn()@text
      })
    },

    #' @description Submit input to the chatbot, returning streaming results.
    #'   Returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream = function(...) {
      turn <- user_turn(...)
      private$chat_impl(turn, stream = TRUE, echo = FALSE)
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream_async = function(...) {
      turn <- user_turn(...)
      private$chat_impl_async(turn, stream = TRUE, echo = FALSE)
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
      if (length(private$.turns) == 0) {
        return(NULL)
      }
      if (private$.turns[[1]]@role != "system") {
        return(NULL)
      }
      private$.turns[[1]]@text
    }
  ),
  private = list(
    provider = NULL,

    .turns = NULL,
    echo = NULL,

    # OpenAI-compliant tool metadata
    tool_infos = NULL,
    # Named list of R functions that implement tools
    tool_funs = NULL,

    add_turn = function(x) {
      if (!S7_inherits(x, turn)) {
        cli::cli_abort("Invalid input", .internal = TRUE)
      }

      private$.turns[[length(private$.turns) + 1]] <- x
      invisible(self)
    },

    add_user_contents = function(contents) {
      stopifnot(is.list(contents))
      if (length(contents) == 0) {
        return(invisible(self))
      }

      i <- length(private$.turns)

      if (private$.turns[[i]]@role != "user") {
        private$.turns[[i + 1]] <- turn("user", content = contents)
      } else {
        private$.turns[[i]]@content <- c(private$.turns[[i]]@content, contents)
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
    # complete assistant turns.
    chat_impl = generator_method(function(self, private, user_turn, stream, echo) {
      while(!is.null(user_turn)) {
        for (chunk in private$submit_turns(user_turn, stream = stream, echo = echo)) {
          yield(chunk)
        }
        user_turn <- private$invoke_tools()
      }

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    chat_impl_async = async_generator_method(function(self, private, user_turn, stream, echo) {
      while(!is.null(user_turn)) {
        for (chunk in await_each(private$submit_turns_async(user_turn, stream = stream, echo = echo))) {
          yield(chunk)
        }
        user_turn <- await(private$invoke_tools_async())
      }

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    submit_turns = generator_method(function(self, private, user_turn, stream, echo) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "stream" else "value",
        turns = c(private$.turns, list(user_turn)),
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
        # Ensure turns always end in a newline
        if (any_text) {
          emit("\n")
          yield("\n")
        }

        turn <- stream_turn(private$provider, result)
      } else {
        turn <- value_turn(private$provider, response)
        text <- turn@text
        if (!is.null(text)) {
          text <- paste0(text, "\n")
          emit(text)
          yield(text)
        }
      }
      private$add_turn(user_turn)
      private$add_turn(turn)

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    submit_turns_async = async_generator_method(function(self, private, user_turn, stream, echo) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "async-stream" else "async-value",
        turns = c(private$.turns, list(user_turn)),
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
        # Ensure turns always end in a newline
        if (any_text) {
          emit("\n")
          yield("\n")
        }

        turn <- stream_turn(private$provider, result)
      } else {
        result <- await(response)

        turn <- value_turn(private$provider, result)
        text <- turn@text
        if (!is.null(text)) {
          text <- paste0(text, "\n")
          emit(text)
          yield(text)
        }
      }
      private$add_turn(user_turn)
      private$add_turn(turn)

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    invoke_tools = function() {
      if (length(private$tool_infos) == 0) {
        return()
      }

      tool_results <- invoke_tools(self$last_turn(), private$tool_funs)
      if (length(tool_results) == 0) {
        return()
      }
      turn("user", tool_results)
    },

    invoke_tools_async = async_method(function(self, private) {
      if (length(private$tool_infos) == 0) {
        return()
      }

      tool_results <- await(invoke_tools_async(self$last_turn(), private$tool_funs))
      if (length(tool_results) == 0) {
        return()
      }
      turn("user", tool_results)
    })
  )
)

#' @export
print.Chat <- function(x, ...) {
  turns <- x$turns(include_system_prompt = TRUE)
  cat(paste0("<Chat turns=", length(turns), ">\n"))
  for (turn in turns) {
    color <- switch(turn@role,
      user = cli::col_blue,
      assistant = cli::col_green,
      system = cli::col_br_white,
      identity
    )
    cli::cat_rule(cli::format_inline("{color(turn@role)}"))
    for (content in turn@content) {
      cat_line(format(content))
    }
  }

  invisible(x)
}
