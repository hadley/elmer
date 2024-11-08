#' @include utils-S7.R
NULL

#' Content types received from and sent to a chatbot
#'
#' @description
#' elmer abstracts away differences in the way that different [Provider]s
#' represent various types of content, allowing you to more easily write
#' code that works with any chatbot.
#'
#' This set of classes represents the various types of content that can be
#' sent to and received from a provider:
#'
#' * `ContentText`: simple text (often in markdown format). This is the only
#'   type of content that can be streamed live as it's received.
#' * `ContentImageRemote` and `ContentImageInline`: images, either as a pointer
#'   to a remote URL or included inline in the object. See
#'   [content_image_file()] and friends for convenient ways to construct these
#'   objects.
#' * `ContentToolRequest`: a request to perform a tool call (sent by the
#'    assistant).
#' * `ContentToolResult`: the result of calling the tool (sent by the user).
#'
#' @export
Content <- new_class("Content")

#' @rdname Content
#' @export
#' @param text A single string.
ContentText <- new_class(
  "ContentText",
  parent = Content,
  properties = list(text = prop_string()),
)
method(format, ContentText) <- function(x, ...) {
  paste0(unlist(strwrap(x@text, width = getOption("width"))), collapse = "\n")
}

# Internal generic for content that has a textual representation.
contents_text <- new_generic("contents_text", "content")

method(contents_text, Content) <- function(content) {
  NULL
}

method(contents_text, ContentText) <- function(content) {
  content@text
}

# Images -----------------------------------------------------------------

#' @rdname Content
#' @export
ContentImage <- new_class(
  "ContentImage",
  parent = Content
)

#' @rdname Content
#' @export
#' @param url URL to a remote image.
#' @param detail Not currently used.
ContentImageRemote <- new_class(
  "ContentImageRemote",
  parent = Content,
  properties = list(
    url = prop_string(),
    detail = prop_string()
  )
)
method(format, ContentImageRemote) <- function(x, ...) {
  cli::format_inline("[{.strong remote image}]: {.url {x@url}}")
}

#' @rdname Content
#' @export
#' @param type MIME type of the image.
#' @param data Base64 encoded image data.
ContentImageInline <- new_class(
  "ContentImageInline",
  parent = Content,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  )
)
method(format, ContentImageInline) <- function(x, ...) {
  cli::format_inline("[{.strong inline image}]")
}

# Tools ------------------------------------------------------------------

#' @rdname Content
#' @export
#' @param id Tool call id (used to associate a request and a result)
#' @param name Function name
#' @param arguments Named list of arguments to call the function with.
ContentToolRequest <- new_class(
  "ContentToolRequest",
  parent = Content,
  properties = list(
    id = prop_string(),
    name = prop_string(),
    arguments = class_list
  )
)
method(format, ContentToolRequest) <- function(x, ...) {
  if (length(x@arguments) == 0) {
    call <- call2(x@name)
  } else {
    call <- call2(x@name, !!!x@arguments)
  }
  cli::format_inline("[{.strong tool request} ({x@id})]: {format(call)}")
}

#' @rdname Content
#' @export
#' @param value,error Either the results of calling the function if
#'   it succeeded, otherwise the error message, as a string. One of
#'   `value` and `error` will always be `NULL`.
ContentToolResult <- new_class(
  "ContentToolResult",
  parent = Content,
  properties = list(
    id = prop_string(),
    value = class_any,
    error = prop_string(allow_null = TRUE)
  )
)
method(format, ContentToolResult) <- function(x, ...) {
  if (tool_errored(x)) {
    value <- paste0(cli::col_red("Error: "), x@error)
  } else {
    value <- x@value
  }
  cli::format_inline("[{.strong tool result}  ({x@id})]: {value}")
}

tool_errored <- function(x) !is.null(x@error)
tool_string <- function(x) {
  if (tool_errored(x)) {
    paste0("Tool calling failed with error ", x@error)
  } else {
    toString(x@value)
  }
}

ContentJson <- new_class(
  "ContentJson",
  parent = Content,
  properties = list(
    value = class_any,
    id = prop_string(allow_null = TRUE)
  )
)
method(format, ContentJson) <- function(x, ...) {
  paste0(
    cli::format_inline("[{.strong data}] "),
    pretty_json(x@value)
  )
}

# Helpers ----------------------------------------------------------------------

as_content <- function(x, error_call = caller_env()) {
  if (is.null(x)) {
    list()
  } else if (is.character(x)) {
    ContentText(paste0(x, collapse = "\n"))
  } else if (S7_inherits(x, Content)) {
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
