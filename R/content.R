#' @include utils-S7.R
NULL

content_image <- new_class("content_image")

content_image_remote <- new_class(
  "content_image_remote",
  parent = content_image,
  properties = list(
    url = prop_string(),
    detail = prop_string()
  ),
  package = "elmer"
)
content_image_inline <- new_class(
  "content_image_inline",
  parent = content_image,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)

# Define allowed types - add new types here in the future
allowed_input_types <- c("text", "image_url")

normalize_chat_input <- function(..., error_call = caller_env()) {
  check_dots_unnamed(call = error_call)
  input <- rlang::list2(...)

  if (length(input) == 1 && is.character(input[[1]])) {
    # The common case of just a string, can be left as a string
    content <- paste(input[[1]], collapse = "\n")
  } else {
    # Otherwise, process all elements
    content <- lapply(input, process_single_input, error_call = error_call)
  }

  list(role = "user", content = content)
}

process_single_input <- function(item, error_call = caller_env()) {
  if (is.character(item)) {
    # If item is a string, convert it to text format
    list(type = "text", text = paste(item, collapse = "\n"))
  } else if (S7_inherits(item, content_image)) {
    item
  } else if (is.list(item)) {
    if (!"type" %in% names(item)) {
      cli::cli_abort("List item must have a 'type' field.", call = error_call)
    }

    type <- item[["type"]]
    type <- arg_match(type, allowed_input_types, error_call = error_call)

    if (!type %in% names(item)) {
      cli::cli_abort("List item of type '{type}' must have a '{type}' field.", call = error_call)
    }

    if (is.null(item[[type]])) {
      cli::cli_abort("'{type}' field cannot be NULL.", call = error_call)
    }

    if (type == "text") {
      item[["text"]] <- paste(item[[type]], collapse = "\n")
    }

    item
  } else {
    stop_input_type(
      item,
      "a string or list",
      arg = I("input content"),
      call = error_call
    )
  }
}
