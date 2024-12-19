#' @include utils-S7.R
NULL

#' A user or assistant turn
#'
#' @description
#' Every conversation with a chatbot consists of pairs of user and assistant
#' turns, corresponding to an HTTP request and response. These turns are
#' represented by the `Turn` object, which contains a list of [Content]s representing
#' the individual messages within the turn. These might be text, images, tool
#' requests (assistant only), or tool responses (user only).
#'
#' Note that a call to `$chat()` and related functions may result in multiple
#' user-assistant turn cycles. For example, if you have registered tools,
#' ellmer will automatically handle the tool calling loop, which may result in
#' any number of additional cycles. Learn more about tool calling in
#' `vignette("tool-calling")`.
#'
#' @param role Either "user", "assistant", or "system".
#' @param contents A list of [Content] objects.
#' @param json The serialized JSON corresponding to the underlying data of
#'   the turns. Currently only provided for assistant.
#'
#'   This is useful if there's information returned by the provider that ellmer
#'   doesn't otherwise expose.
#' @param tokens A numeric vector of length 2 representing the number of
#'   input and output tokens (respectively) used in this turn. Currently
#'   only recorded for assistant turns.
#' @export
#' @return An S7 `Turn` object
#' @examples
#' Turn(role = "user", contents = list(ContentText("Hello, world!")))
Turn <- new_class(
  "Turn",
  properties = list(
    role = prop_string(),
    contents = prop_list_of(Content),
    json = class_list,
    tokens = new_property(
      class_numeric,
      default = c(NA_real_, NA_real_),
      validator = function(value) {
        if (length(value) != 2) {
          "must be length two"
        }
      }
    ),
    text = new_property(
      class = class_character,
      getter = function(self) contents_text(self)
    )
  ),
  constructor = function(role,
                         contents = list(),
                         json = list(),
                         tokens = c(0, 0)) {

   if (is.character(contents)) {
      contents <- list(ContentText(paste0(contents, collapse = "\n")))
    }
    new_object(
      S7_object(),
      role = role,
      contents = contents,
      json = json,
      tokens = tokens
    )
  }
)
method(format, Turn) <- function(x, ...) {
  contents <- map_chr(x@contents, format, ...)
  paste0(contents, "\n", collapse = "")
}
method(contents_text, Turn) <- function(content) {
  paste0(unlist(lapply(content@contents, contents_text)), collapse = "")
}
method(contents_html, Turn) <- function(content) {
  paste0(unlist(lapply(content@contents, contents_html)), collapse = "\n")
}
method(contents_markdown, Turn) <- function(content) {
  paste0(unlist(lapply(content@contents, contents_markdown)), collapse = "\n\n")
}

user_turn <- function(..., .error_call = caller_env()) {
  if (...length() == 0) {
    cli::cli_abort("Must supply at least one input.", error_call = .error_call)
  }

  check_dots_unnamed(call = .error_call)
  input <- list2(...)
  contents <- lapply(input, as_content, error_call = .error_call)

  Turn("user", contents)
}

is_system_prompt <- function(x) {
  x@role == "system"
}

normalize_turns <- function(turns = NULL,
                            system_prompt = NULL,
                            overwrite = FALSE,
                            error_call = caller_env()) {

  check_character(system_prompt, allow_null = TRUE, call = error_call)
  if (length(system_prompt) > 1) {
    system_prompt <- paste(system_prompt, collapse = "\n\n")
  }

  if (!is.null(turns)) {
    if (!is.list(turns) || is_named(turns)) {
      stop_input_type(
        turns,
        "an unnamed list",
        allow_null = TRUE,
        call = error_call
      )
    }
    correct_class <- map_lgl(turns, S7_inherits, Turn)
    if (!all(correct_class)) {
      cli::cli_abort("Every element of {.arg turns} must be a `turn`.")
    }
  } else {
    turns <- list()
  }

  if (!is.null(system_prompt)) {
    system_turn <- Turn("system", system_prompt)

    # No turns; start with just the system prompt
    if (length(turns) == 0) {
      turns <- list(system_turn)
    } else if (turns[[1]]@role != "system") {
      turns <- c(list(system_turn), turns)
    } else if (overwrite || identical(turns[[1]], system_turn)) {
      # Duplicate system prompt; don't need to do anything
    } else {
      cli::cli_abort(
        "`system_prompt` and `turns[[1]]` can't contain conflicting system prompts.",
        call = error_call
      )
    }
  }

  turns
}
