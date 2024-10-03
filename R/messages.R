# Abstract class
chat_message <- new_class(
  "chat_message",
  properties = list(
    role = prop_string(),
    # list_of<content
    content = class_list,
    # random extra metadata a provider stuffs in
    extra = class_list
  ),
  constructor = function(role, content = list(), extra = list()) {
    if (is.character(content)) {
      content <- list(content_text(paste0(content, collapse = "\n")))
    }
    new_object(S7_object(), role = role, content = content, extra = extra)
  }
)

method(format, chat_message) <- function(x, ...) {
  contents <- map_chr(x@content, format, ...)
  paste0(contents, "\n", collapse = "")
}

user_message <- function(..., .error_call = caller_env()) {
  check_dots_unnamed(call = .error_call)
  input <- list2(...)
  content <- lapply(input, as_content, error_call = .error_call)

  chat_message(role = "user", content = content)
}

as_content <- function(x, error_call = caller_env()) {
  if (is.null(x)) {
    list()
  } else if (is.character(x)) {
    content_text(x)
  } else if (inherits(x, content)) {
    x
  } else {
    stop_input_type(
      x,
      what = "made up strings or <content> objects",
      arg = "...",
      error_call = error_call
    )
  }
}

is_system_prompt <- function(x) {
  x@role == "system"
}

normalize_messages <- function(messages = NULL,
                               system_prompt = NULL,
                               error_call = caller_env()) {

  check_string(system_prompt, allow_null = TRUE, call = error_call)

  if (!is.null(messages)) {
    if (!is.list(messages) || is_named(messages)) {
      stop_input_type(
        messages,
        "an unnamed list",
        allow_null = TRUE,
        call = error_call
      )
    }
    correct_class <- map_lgl(messages, inherits, chat_message)
    if (!all(correct_class)) {
      cli::cli_abort("Every element of {.arg messages} must be a `chat_message`.")
    }
  } else {
    messages <- list()
  }

  if (!is.null(system_prompt)) {
    system_message <- chat_message("system", system_prompt)

    # No messages; start with just the system prompt
    if (length(messages) == 0) {
      messages <- list(system_message)
    } else if (messages[[1]]@role != "system") {
      messages <- c(list(system_message), messages)
    } else if (identical(messages[[1]], system_message)) {
      # Duplicate system prompt; don't need to do anything
    } else {
      cli::cli_abort(
        "`system_prompt` and `messages[[1]]` can't contain conflicting system prompts.",
        call = error_call
      )
    }
  }

  messages
}
