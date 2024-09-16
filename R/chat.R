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
#' @param messages A list of messages to start the chat with (i.e., continuing a
#'   previous conversation). If not provided, the conversation begins from
#'   scratch. Do not provide non-`NULL` values for both `messages` and
#'   `system_prompt`.
#'
#'   Each message in the list should be a named list with at least `role`
#'   (usually `system`, `user`, or `assistant`, but `tool` is also possible).
#'   Normally there is also a `content` field, which is a string.
#' @param base_url The base URL to the endpoint; the default uses OpenAI.
#' @param api_key The API key to use for authentication. You generally should
#'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
#'   variable.
#' @param model The model to use for the chat; set to `NULL` (the default) to
#'   use a reasonable model, currently `gpt-4o-mini`. We strongly recommend
#'   explicitly choosing a model for all but the most casual use.
#' @param echo If `TRUE`, the `chat()` method streams the response to stdout by
#'   default. (Note that this has no effect on the `stream()`, `chat_async()`,
#'   and `stream_async()` methods.)
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
                            messages = NULL,
                            base_url = "https://api.openai.com/v1",
                            api_key = openai_key(),
                            model = NULL,
                            echo = FALSE) {
  check_string(system_prompt, allow_null = TRUE)
  check_openai_conversation(messages, allow_null = TRUE)
    check_string(base_url)
  check_string(api_key)
  check_string(model, allow_null = TRUE, allow_na = TRUE)
  check_bool(echo)

  model <- model %||% "gpt-4o-mini"

  messages <- apply_system_prompt_openai(system_prompt, messages)

  ChatOpenAI$new(
    base_url = base_url,
    model = model,
    messages = messages,
    api_key = api_key,
    echo = echo
  )
}

apply_system_prompt_openai <- function(system_prompt, messages) {
  if (is.null(system_prompt)) {
    return(messages)
  }

  system_prompt_message <- list(
    role = "system",
    content = system_prompt
  )

  # No messages; start with just the system prompt
  if (length(messages) == 0) {
    return(list(system_prompt_message))
  }

  # No existing system prompt message; prepend the new one
  if (messages[[1]][["role"]] != "system") {
    return(c(list(system_prompt_message), messages))
  }

  # Duplicate system prompt; return as-is
  if (messages[[1]][["content"]] == system_prompt) {
    return(messages)
  }

  stop("`system_prompt` and `messages[[1]]` contained conflicting system prompts")
}

check_openai_conversation <- function(messages, allow_null = FALSE) {
  if (is.null(messages) && isTRUE(allow_null)) {
    return()
  }

  if (!is.list(messages) ||
      !(is.null(names(messages)) || all(names(messages) == ""))) {
    stop_input_type(
      messages,
      "an unnamed list of messages",
      allow_null = FALSE
    )
  }

  for (message in messages) {
    if (!is.list(message) ||
        !is.character(message$role)) {
      stop("Each message must be a named list with at least a `role` field.")
    }
  }
}

#' @rdname new_chat_openai
ChatOpenAI <- R6::R6Class("ChatOpenAI",
  public = list(
    #' @param base_url The base URL to the endpoint; the default uses ChatGPT.
    #' @param api_key The API key to use for authentication. You generally should
    #'   not supply this directly, but instead set the `OPENAI_API_KEY` environment
    #'   variable.
    #' @param model The model to use for the chat; defaults to GPT-4o mini.
    #' @param messages An unnamed list of messages to start the chat with (i.e.,
    #'   continuing a previous conversation). If `NULL` or zero-length list, the
    #'   conversation begins from scratch.
    #' @param echo If `TRUE`, the `chat()` method streams the response to stdout
    #'   (while also returning the final response). Note that this has no effect
    #'   on the `stream()`, `chat_async()`, and `stream_async()` methods.
    initialize = function(base_url, model, messages, api_key, echo = FALSE) {
      private$base_url <- base_url
      private$model <- model
      private$msgs <- messages %||% list()
      private$api_key <- api_key
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

    #' @description Submit text to the chatbot, and return the response as a
    #'   simple string (probably Markdown).
    #' @param text The text to send to the chatbot.
    #' @param echo Whether to emit the response to stdout as it is received. If
    #'   `NULL`, then the value of `echo` set when the chat object was created
    #'   will be used.
    chat = function(text, echo = NULL) {
      check_string(text)

      echo <- echo %||% private$echo

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      coro::collect(private$chat_impl(text, stream = echo, echo = echo))
      last_message <- private$msgs[[length(private$msgs)]]
      stopifnot(identical(last_message[["role"]], "assistant"))

      if (echo) {
        invisible(last_message$content)
      } else {
        last_message$content
      }
    },

    #' @description Submit text to the chatbot, and receive a promise that
    #'   resolves with the response all at once.
    #' @param text The text to send to the chatbot.
    #' @returns A promise that resolves to a string (probably Markdown).
    chat_async = function(text) {
      check_string(text)

      # Returns a single message (the final response from the assistant), even if
      # multiple rounds of back and forth happened.
      done <- coro::async_collect(
        private$chat_impl_async(text, stream = FALSE, echo = FALSE)
      )
      promises::then(done, function(dummy) {
        last_message <- private$msgs[[length(private$msgs)]]
        stopifnot(identical(last_message[["role"]], "assistant"))
        last_message$content
      })
    },

    #' @description Submit text to the chatbot, returning streaming results.
    #'   Returns A [coro
    #'   generator](https://coro.r-lib.org/articles/generator.html#iterating)
    #'   that yields strings. While iterating, the generator will block while
    #'   waiting for more content from the chatbot.
    #' @param text The text to send to the chatbot.
    stream = function(text) {
      private$chat_impl(text, stream = TRUE, echo = FALSE)
    },

    #' @description Submit text to the chatbot, returning asynchronously
    #'   streaming results. Returns a [coro async
    #'   generator](https://coro.r-lib.org/reference/async_generator.html) that
    #'   yields string promises.
    #' @param text The text to send to the chatbot.
    stream_async = function(text) {
      private$chat_impl_async(text, stream = TRUE, echo = FALSE)
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
        self$chat(user_input, echo = TRUE)
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
  active = list(
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
    base_url = NULL,
    model = NULL,
    api_key = NULL,

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
    chat_impl = generator_method(function(self, private, text, stream, echo) {
      private$add_message(list(role = "user", content = text))
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
    chat_impl_async = async_generator_method(function(self, private, text, stream, echo) {
      private$add_message(list(role = "user", content = text))
      while (TRUE) {
        for (chunk in await_each(private$submit_messages_async(stream = stream, echo = echo))) {
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
    submit_messages = generator_method(function(self, private, stream, echo) {
      response <- openai_chat(
        messages = private$msgs,
        tools = private$tool_infos,
        base_url = private$base_url,
        model = private$model,
        stream = stream,
        api_key = private$api_key
      )

      if (echo) {
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
    submit_messages_async = async_generator_method(function(self, private, stream, echo) {
      response <- openai_chat_async(
        messages = private$msgs,
        tools = private$tool_infos,
        base_url = private$base_url,
        model = private$model,
        stream = stream,
        api_key = private$api_key
      )

      if (echo) {
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
        last_message <- private$msgs[[length(private$msgs)]]
        tool_message <- call_tools(private$tool_funs, last_message)

        if (!is.null(tool_message)) {
          private$msgs <- c(private$msgs, tool_message)
          return(TRUE)
        }
      }
      FALSE
    }
  )
)

#' @export
print.ChatOpenAI <- function(x, ...) {
  msgs <- x$messages(include_system_prompt = TRUE)
  msgs_without_system_prompt <- x$messages(include_system_prompt = FALSE)
  cat(paste0("<ChatOpenAI messages=", length(msgs_without_system_prompt), ">\n"))
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
      cli::cli_text("{message$content}")
    }
    if (!is.null(message$tool_calls)) {
      cli::cli_text("Tool calls:")
      for (tool_call in message$tool_calls) {
        funcname <- tool_call$`function`$name
        args <- tool_call$`function`$arguments
        tryCatch({
          args_parsed <- jsonlite::parse_json(tool_call$`function`$arguments)
          args <- rlang::call2(funcname, !!!args_parsed)
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


last_message <- function(chat) {
  messages <- chat$messages()
  messages[[length(messages)]]
}
