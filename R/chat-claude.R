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
#' chat <- new_chat_claude()
#' chat$chat("
#'   What is the difference between a tibble and a data frame?
#'   Answer with a bulleted list
#' ")
#'
#' chat <- new_chat_openai()
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
#' ")
new_chat_claude <- function(system_prompt = NULL,
                            messages = NULL,
                            base_url = claude_base_url,
                            api_key = claude_key(),
                            model = NULL,
                            echo = FALSE) {
  check_string(system_prompt, allow_null = TRUE)
  check_openai_conversation(messages, allow_null = TRUE)
  check_string(base_url)
  check_string(api_key)
  check_string(model, allow_null = TRUE, allow_na = TRUE)
  check_bool(echo)

  model <- model %||% claude_model

  ChatClaude$new(
    base_url = base_url,
    model = model,
    system_prompt = system_prompt,
    messages = messages,
    api_key = api_key,
    echo = echo
  )
}

#' @rdname new_chat_openai
ChatClaude <- R6::R6Class("ChatClaude",
  public = list(
    system_prompt = NULL,

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
    initialize = function(base_url, model, system_prompt, messages, api_key, echo = FALSE) {
      private$base_url <- base_url
      private$model <- model
      self$system_prompt <- system_prompt
      private$msgs <- messages %||% list()
      private$api_key <- api_key
      private$echo <- echo
    },

    #' @description The messages that have been sent and received so far
    #'   (optionally starting with the system prompt, if any).
    #' @param include_system_prompt Whether to include the system prompt in the
    #'   messages (if any exists).
    messages = function() {
      private$msgs
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
    register_tool = function(fun, name, description, arguments, strict = FALSE) {
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
      response <- claude_chat(
        messages = private$msgs,
        system_prompt = self$system_prompt,
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
        # https://docs.anthropic.com/en/api/messages-streaming#basic-streaming-request
        for (event in response) {

          if (event$type == "ping") {
            next
          } else if (event$type == "message_start") {
            result$role <- event$message$role
          } else if (event$type == "content_block_start") {
            result$content[[event$index + 1L]] <- event$content_block
          } else if (event$type == "content_block_delta") {
            result$content[[event$index + 1L]]$text <- paste0(result$content[[event$index + 1L]]$text, event$delta$text)
            emit(event$delta$text)
            yield(event$delta$text)
          } else if (event$type == "content_block_stop") {
            emit("\n")
            yield("\n")
          } else if (event$type == "message_delta") {
            # TODO: do something with stop reason
          } else {
            browser()
          }
        }
        private$add_message(result)
      } else {
        message <- list(
          role = response$role,
          content = response$content
        )
        private$add_message(message)

        # Print response if its text
        if (!is.null(response$content[[1]]$text)) {
          emit(response$content[[1]]$text)
          emit("\n")
          yield(response$content[[1]]$text)
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
