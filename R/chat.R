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
#'     tool_arg(
#'       "n",
#'       type = "integer",
#'       description = "The number of observations. Must be a positive integer."
#'     ),
#'     tool_arg(
#'       "mean",
#'       type = "number",
#'       description = "The mean value of the distribution."
#'     ),
#'     tool_arg(
#'       "sd",
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

  Chat$new(
    base_url = base_url,
    model = model,
    api_key = api_key,
    system_prompt = system_prompt,
    quiet = quiet
  )
}

#' @rdname new_chat_openai
Chat <- R6::R6Class("Chat",
  public = list(
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

    #' @description Submit text to the chatbot.
    #' @param text The text to send to the chatbot
    #' @param stream Whether to stream the response or not.
    chat = function(text, stream = TRUE) {
      check_string(text)
      check_bool(stream)

      private$add_message(list(role = "user", content = text))
      private$submit_messages(stream = stream)
      private$tool_loop()
      invisible(self)
    },

    #' @description Register a tool (an R function) that the chatbot can use.
    #'   If the chatbot decides to use the function, elmer will automatically
    #'   call it and submit the results back.
    #' @param name The name of the function.
    #' @param description A detailed description of what the function does.
    #'   Generally, the more information that you can provide here, the better.
    #' @param arguments A list of arguments that the function accepts.
    #'   Should be a list of objects created by [tool_arg()].
    #' @param strict Should the argument definition be strictly enforced?
    register_tool = function(name, description, arguments, strict = TRUE) {
      check_string(name)
      check_string(description)
      check_bool(strict)

      tool <- tool_def(
        name = name,
        description = description,
        arguments = arguments,
        strict = strict
      )
      private$add_tool(tool)
      invisible(self)
    }
  ),
  private = list(
    base_url = NULL,
    model = NULL,
    api_key = NULL,

    messages = NULL,
    tools = NULL,
    quiet = NULL,

    add_message = function(message) {
      private$messages <- c(private$messages, list(message))
      invisible(self)
    },

    add_tool = function(tool) {
      private$tools <- c(private$tools, list(tool))
      invisible(self)
    },

    submit_messages = function(stream = TRUE) {
      result <- openai_chat(
        messages = private$messages,
        tools = private$tools,
        base_url = private$base_url,
        model = private$model,
        stream = stream,
        api_key = private$api_key,
        quiet = private$quiet
      )
      if (stream) {
        private$add_message(result$choices[[1]]$delta)
      } else {
        private$add_message(result$choices[[1]]$message)
      }

      invisible(self)
    },

    tool_loop = function() {
      if (is.null(private$tools)) {
        return()
      }

      last_message <- private$messages[[length(private$messages)]]
      tool_message <- call_tools(last_message)

      if (is.null(tool_message)) {
        return()
      }
      private$messages <- c(private$messages, tool_message)
      private$submit_messages(stream = FALSE)
    }
  )
)


last_message <- function(chat) {
  messages <- chat$.__enclos_env__$private$messages
  messages[[length(messages)]]
}
