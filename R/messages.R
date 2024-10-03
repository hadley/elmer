# Abstract class
chat_message <- new_class(
  "chat_message",
  properties = list(
    role = prop_string(),
    # list_of<content
    content = class_list,
    # random extra metadata a provider stuffs in
    extra = class_list
  )
)

method(format, chat_message) <- function(x, ...) {
  contents <- map_chr(x@content, format, ...)
  paste0(contents, "\n", collapse = "")
}

user_message <- function(...) {
  check_dots_unnamed(call = error_call)
  input <- list2(...)
  content <- lapply(input, as_content)

  chat_message(role = "user", content = content)
}

as_content <- function(x) {
  if (is.null(x)) {
    list()
  } else if (is.character(x)) {
    content_text(x)
  } else if (inherits(x, content)) {
    x
  } else {
    cli::cli_abort("Unknown input", .internal = TRUE)
  }
}

is_system_prompt <- function(x) {
  x@role == "system"
}
