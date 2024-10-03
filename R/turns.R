turn <- new_class(
  "turn",
  properties = list(
    role = prop_string(),
    # list_of<content
    content = class_list,
    # random extra metadata a provider stuffs in
    extra = class_list,
    text = new_property(
      class = class_character,
      getter = function(self) {
        is_text <- map_lgl(self@content, S7_inherits, content_text)
        paste0(unlist(lapply(self@content[is_text], function(x) x@text)), collapse = "")
      }
    )
  ),
  constructor = function(role, content = list(), extra = list()) {
    if (is.character(content)) {
      content <- list(content_text(paste0(content, collapse = "\n")))
    }
    new_object(S7_object(), role = role, content = content, extra = extra)
  }
)
method(format, turn) <- function(x, ...) {
  contents <- map_chr(x@content, format, ...)
  paste0(contents, "\n", collapse = "")
}



user_turn <- function(..., .error_call = caller_env()) {
  check_dots_unnamed(call = .error_call)
  input <- list2(...)
  content <- lapply(input, as_content, error_call = .error_call)

  turn(role = "user", content = content)
}

is_system_prompt <- function(x) {
  x@role == "system"
}

normalize_turns <- function(turns = NULL,
                               system_prompt = NULL,
                               error_call = caller_env()) {

  check_string(system_prompt, allow_null = TRUE, call = error_call)

  if (!is.null(turns)) {
    if (!is.list(turns) || is_named(turns)) {
      stop_input_type(
        turns,
        "an unnamed list",
        allow_null = TRUE,
        call = error_call
      )
    }
    correct_class <- map_lgl(turns, S7_inherits, turn)
    if (!all(correct_class)) {
      cli::cli_abort("Every element of {.arg turns} must be a `turn`.")
    }
  } else {
    turns <- list()
  }

  if (!is.null(system_prompt)) {
    system_turn <- turn("system", system_prompt)

    # No turns; start with just the system prompt
    if (length(turns) == 0) {
      turns <- list(system_turn)
    } else if (turns[[1]]@role != "system") {
      turns <- c(list(system_turn), turns)
    } else if (identical(turns[[1]], system_turn)) {
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
