#' @include utils-S7.R
NULL

Content <- new_class("Content", package = "elmer")

ContentText <- new_class(
  "ContentText",
  parent = Content,
  properties = list(text = prop_string()),
  package = "elmer"
)
method(format, ContentText) <- function(x, ...) {
  paste0(unlist(strwrap(x@text, width = getOption("width"))), collapse = "\n")
}

# Images -----------------------------------------------------------------

ContentImage <- new_class(
  "ContentImage",
  parent = Content,
  package = "elmer"
)

ContentImageRemote <- new_class(
  "ContentImageRemote",
  parent = Content,
  properties = list(
    url = prop_string(),
    detail = prop_string()
  ),
  package = "elmer"
)
method(format, ContentImageRemote) <- function(x, ...) {
  cli::format_inline("[{.strong remote image}]: {.url {x@url}}")
}

ContentImageInline <- new_class(
  "ContentImageInline",
  parent = Content,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
method(format, ContentImageInline) <- function(x, ...) {
  cli::format_inline("[{.strong inline image}]")
}

# Tools ------------------------------------------------------------------

ContentToolRequest <- new_class(
  "ContentToolRequest",
  parent = Content,
  properties = list(
    id = prop_string(),
    name = prop_string(),
    arguments = class_list,
    parse_error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
method(format, ContentToolRequest) <- function(x, ...) {
  if (length(x@arguments) == 0) {
    call <- call2(x@name)
  } else {
    call <- call2(x@name, x@arguments)
  }
  cli::format_inline("[{.strong tool request} ({x@id})]: {format(call)}")
}

ContentToolResult <- new_class(
  "ContentToolResult",
  parent = Content,
  properties = list(
    id = prop_string(),
    result = class_any,
    error = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
method(format, ContentToolResult) <- function(x, ...) {
  cli::format_inline("[{.strong tool result}  ({x@id})]: {x@result}")
}

tool_errored <- function(x) !is.null(x@error)
tool_string <- function(x) {
  if (tool_errored(x)) {
    paste0("Tool calling failed with error ", x@error)
  } else {
    toString(x@result)
  }
}

# Helpers ----------------------------------------------------------------------

as_content <- function(x, error_call = caller_env()) {
  if (is.null(x)) {
    list()
  } else if (is.character(x)) {
    ContentText(x)
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
