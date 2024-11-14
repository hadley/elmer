#' @include utils-coro.R
NULL

#' A chat
#'
#' @description
#' A `Chat` is an sequence of sequence of user and assistant [Turn]s sent
#' to a specific [Provider]. A `Chat` is a mutable R6 object that takes care of
#' managing the state associated with the chat; i.e. it records the messages
#' that you send to the server, and the messages that you receive back.
#' If you register a tool (i.e. an R function that the assistant can call on
#' your behalf), it also takes care of the tool loop.
#'
#' You should generally not create this object yourself,
#' but instead call [chat_openai()] or friends instead.
Chat <- R6::R6Class("Chat",
  public = list(
    #' @param provider A provider object.
    #' @param turns An unnamed list of turns to start the chat with (i.e.,
    #'   continuing a previous conversation). If `NULL` or zero-length list, the
    #'   conversation begins from scratch.
    #' @param seed Optional integer seed that ChatGPT uses to try and make output
    #'   more reproducible.
    #' @param echo One of the following options:
    #'   * `none`: don't emit any output (default when running in a function).
    #'   * `text`: echo text output as it streams in (default when running at
    #'     the console).
    #'   * `all`: echo all input and output.
    #'
    #'  Note this only affects the `chat()` method.
    initialize = function(provider, turns, seed = NULL, echo = "none") {
      private$provider <- provider
      private$.turns <- turns %||% list()
      private$echo <- echo
    },

    #' @description Retrieve the turns that have been sent and received so far
    #'   (optionally starting with the system prompt, if any).
    #' @param include_system_prompt Whether to include the system prompt in the
    #'   turns (if any exists).
    get_turns = function(include_system_prompt = FALSE) {
      if (length(private$.turns) == 0) {
        return(private$.turns)
      }

      if (!include_system_prompt && is_system_prompt(private$.turns[[1]])) {
        private$.turns[-1]
      } else {
        private$.turns
      }
    },

    #' @description Replace existing turns with a new list.
    #' @param value A list of [Turn]s.
    set_turns = function(value) {
      private$.turns <- normalize_turns(value, self$system_prompt)
      invisible(self)
    },

    #' @description If set, the system prompt, it not, `NULL`.
    get_system_prompt = function() {
      if (private$has_system_prompt()) {
        private$.turns[[1]]@text
      } else {
        NULL
      }
    },

    #' @description Update the system prompt
    #' @param value A string giving the new system prompt
    set_system_prompt = function(value) {
      check_string(value, allow_null = TRUE)
      # Remove prompt, if present
      if (private$has_system_prompt()) {
        private$.turns <- private$.turns[-1]
      }
      # Add prompt, if new
      if (is.character(value)) {
        private$.turns <- c(list(Turn("system", value)), private$.turns)
      }
      invisible(self)
    },

    #' @description List the number of tokens consumed by each assistant turn.
    #'   Currently tokens are recorded for assistant turns only; so user
    #'   turns will have zeros.
    tokens = function() {
      tokens <- vapply(private$.turns, function(turn) turn@tokens, double(2))
      tokens <- t(tokens)
      colnames(tokens) <- c("input", "output")
      tokens
    },

    #' @description The last turn returned by the assistant.
    #' @param role Optionally, specify a role to find the last turn with
    #'   for the role.
    #' @return Either a `Turn` or `NULL`, if no turns with the specified
    #'   role have occurred.
    last_turn = function(role = c("assistant", "user", "system")) {
      role <- arg_match(role)

      n <- length(private$.turns)
      switch(role,
        system = if (private$has_system_prompt()) private$.turns[[1]],
        assistant = if (n > 1) private$.turns[[n]],
        user = if (n > 1) private$.turns[[n - 1]]
      )
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
      echo <- check_echo(echo %||% private$echo)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(turn, stream = echo != "none", echo = echo))

      text <- self$last_turn()@text
      if (echo == "none") text else invisible(text)
    },

    #' @description Extract structured data
    #' @param ... The input to send to the chatbot. Will typically include
    #'   the phrase "extract structured data".
    #' @param spec A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'  all providers).
    extract_data = function(..., spec, echo = "none") {
      turn <- user_turn(...)
      echo <- check_echo(echo %||% private$echo)

      coro::collect(private$submit_turns(
        turn,
        spec = spec,
        stream = echo != "none",
        echo = echo
      ))

      turn <- self$last_turn()
      is_json <- map_lgl(turn@contents, S7_inherits, ContentJson)
      n <- sum(is_json)
      if (n != 1) {
        cli::cli_abort("Data extraction failed: {n} data results recieved.")
      }

      json <- turn@contents[[which(is_json)]]
      json@value
    },

    #' @description Extract structured data, asynchronously. Returns a promise
    #'   that resolves to an object matching the type specification.
    #' @param ... The input to send to the chatbot. Will typically include
    #'   the phrase "extract structured data".
    #' @param spec A type specification for the extracted data. Should be
    #'   created with a [`type_()`][type_boolean] function.
    #' @param echo Whether to emit the response to stdout as it is received.
    #'   Set to "text" to stream JSON data as it's generated (not supported by
    #'  all providers).
    extract_data_async = function(..., spec, echo = "none") {
      turn <- user_turn(...)
      echo <- check_echo(echo %||% private$echo)

      done <- coro::async_collect(private$submit_turns_async(
        turn,
        spec = spec,
        stream = echo != "none",
        echo = echo
      ))
      promises::then(done, function(dummy) {
        turn <- self$last_turn()
        is_json <- map_lgl(turn@contents, S7_inherits, ContentJson)
        n <- sum(is_json)
        if (n != 1) {
          cli::cli_abort("Data extraction failed: {n} data results recieved.")
        }

        json <- turn@contents[[which(is_json)]]
        json@value
      })
    },

    #' @description Submit input to the chatbot, and receive a promise that
    #'   resolves with the response all at once. Returns a promise that resolves
    #'   to a string (probably Markdown).
    #' @param ... The input to send to the chatbot. Can be strings or images.
    chat_async = function(...) {
      turn <- user_turn(...)

      # Returns a single turn (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(turn, stream = FALSE, echo = "none")
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
      private$chat_impl(turn, stream = TRUE, echo = "none")
    },

    #' @description Submit input to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param ... The input to send to the chatbot. Can be strings or images.
    stream_async = function(...) {
      turn <- user_turn(...)
      private$chat_impl_async(turn, stream = TRUE, echo = "none")
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   If the chatbot decides to use the function,  elmer will automatically
    #'   call it and submit the results back.
    #' @param tool_def Tool definition created by [tool()].
    register_tool = function(tool_def) {
      if (!S7_inherits(tool_def, ToolDef)) {
        cli::cli_abort("{.arg tool} must be a <ToolDef>.")
      }

      private$tools[[tool_def@name]] <- tool_def
      invisible(self)
    }
  ),
  private = list(
    provider = NULL,

    .turns = NULL,
    echo = NULL,
    tools = list(),

    add_turn = function(x) {
      if (!S7_inherits(x, Turn)) {
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
        private$.turns[[i + 1]] <- Turn("user", contents)
      } else {
        private$.turns[[i]]@contents <- c(private$.turns[[i]]@contents, contents)
      }
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
        if (echo == "all") {
          cat(format(user_turn))
        }
      }

      # Work around https://github.com/r-lib/coro/issues/51
      if (FALSE) {
        yield(NULL)
      }
    }),

    # If stream = TRUE, yields completion deltas. If stream = FALSE, yields
    # complete assistant turns.
    submit_turns = generator_method(function(self, private, user_turn, stream, echo, spec = NULL) {

      if (echo == "all") {
        cat_line(format(user_turn), prefix = "> ")
      }

      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "stream" else "value",
        turns = c(private$.turns, list(user_turn)),
        tools = private$tools,
        spec = spec
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
        turn <- value_turn(private$provider, result, has_spec = !is.null(spec))

        # Ensure turns always end in a newline
        if (any_text) {
          emit("\n")
          yield("\n")
        }

        if (echo == "all") {
          is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
          formatted <- map_chr(turn@contents[!is_text], format)
          cat_line(formatted, prefix = "< ")
        }
      } else {
        turn <- value_turn(private$provider, response, has_spec = !is.null(spec))
        text <- turn@text
        if (!is.null(text)) {
          text <- paste0(text, "\n")
          emit(text)
          yield(text)
        }
        if (echo == "all") {
          cat_line(format(turn), prefix = "< ")
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
    submit_turns_async = async_generator_method(function(self, private, user_turn, stream, echo, spec = NULL) {
      response <- chat_perform(
        provider = private$provider,
        mode = if (stream) "async-stream" else "async-value",
        turns = c(private$.turns, list(user_turn)),
        tools = private$tools,
        spec = spec
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
        turn <- value_turn(private$provider, result, has_spec = !is.null(spec))

        # Ensure turns always end in a newline
        if (any_text) {
          emit("\n")
          yield("\n")
        }
      } else {
        result <- await(response)

        turn <- value_turn(private$provider, result, has_spec = !is.null(spec))
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
      tool_results <- invoke_tools(self$last_turn(), private$tools)
      if (length(tool_results) == 0) {
        return()
      }
      Turn("user", tool_results)
    },

    invoke_tools_async = async_method(function(self, private) {
      tool_results <- await(invoke_tools_async(self$last_turn(), private$tools))
      if (length(tool_results) == 0) {
        return()
      }
      Turn("user", tool_results)
    }),

    has_system_prompt = function() {
      length(private$.turns) > 0 && private$.turns[[1]]@role == "system"
    }
  )
)

#' @export
print.Chat <- function(x, ...) {
  turns <- x$get_turns(include_system_prompt = TRUE)
  tokens <- colSums(x$tokens())
  cat(paste0("<Chat turns=", length(turns), " tokens=", tokens[1], "/", tokens[2], ">\n"))
  for (turn in turns) {
    color <- switch(turn@role,
      user = cli::col_blue,
      assistant = cli::col_green,
      system = cli::col_br_white,
      identity
    )
    cli::cat_rule(cli::format_inline("{color(turn@role)}"))
    for (content in turn@contents) {
      cat_line(format(content))
    }
  }

  invisible(x)
}
