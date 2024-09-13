call_tools <- function(tools, message) {
  if (!has_name(message, "tool_calls")) {
    return()
  }

  lapply(message$tool_calls, function(call) {
    fun <- call$`function`
    tool_fun <- tools[[fun$name]]
    if (is.null(tool_fun)) {
      result <- paste0("Error calling tool: Unknown tool name '", call$`function`, "'")
    } else {
      args <- jsonlite::fromJSON(fun$arguments)
      result <- call_tool(tool_fun, args)
    }

    list(
      role = "tool",
      content = toString(result),
      tool_call_id = call$id
    )
  })
}

# Should we check `fun` against registered tools?
# Also need to handle edge caess: https://platform.openai.com/docs/guides/function-calling/edge-cases
call_tool <- function(fun, arguments) {
  tryCatch(
    do.call(fun, arguments),
    error = function(e) {
      # TODO: We need to report this somehow; it's way too hidden from the user
      paste0("Error calling tool: ", conditionMessage(e))
    }
  )
}

get_help_text <- function(topic, package = NULL) {
  file_contents <- NULL
  fake_pager <- function(files, header, title, delete.file) {
    if (delete.file) {
      on.exit(unlink(files))
    }

    for (file in files) {
      file_contents <<- c(
        file_contents,
        readLines(files, warn = FALSE),
        "\n"
      )
    }
  }

  op <- options(help_type = "text", pager = fake_pager)
  on.exit(options(op), add = TRUE)
  rd_opts <- tools::Rd2txt_options(underline_titles = FALSE)
  on.exit(tools::Rd2txt_options(rd_opts), add = TRUE)

  help_files <- utils::help((topic), package = (package))
  if (length(help_files) == 0) {
    cli::cli_abort("No help files found")
  }

  # Has side effect of setting file_contents
  print(help_files)

  paste(file_contents, collapse = "\n")
}

#' Create metadata for a tool
#'
#' @description In order to use a function as a tool in a chat, you need to
#' craft the right metadata to pass to `register_tool()`. This function helps
#' you do that for documented package functions by extracting the function's R
#' documentation and creating a `register_tool` call for you, using an LLM. It's
#' meant to be used interactively while writing your code, not as part of your
#' final code.
#'
#' Note that this function is inherently imperfect. It can't handle all possible
#' R functions, because not all parameters are suitable for use in a tool call
#' (for example, because they're not serializable to simple JSON objects). The
#' documentation might not specify the expected shape of arguments to the level
#' of detail that would allow an exact JSON schema to be generated.
#'
#' @param topic A symbol or string literal naming the function to create
#'   metadata for. Can also be an expression of the form `pkg::fun`.
#' @param model The OpenAI model to use for generating the metadata. Defaults to
#'  "gpt-4o", which is highly recommended over "gpt-4o-mini".
#' @param echo Emit the registration code to the console. Defaults to `TRUE` in
#'   interactive sessions.
#'
#' @return A `register_tool` call that you can copy and paste into your code.
#'   Returned invisibly if `echo` is `TRUE`.
#'
#' @export
create_tool_metadata <- function(topic, model = "gpt-4o", echo = interactive()) {
  expr <- rlang::enexpr(topic)

  pkg <- NULL
  fun <- format(expr)

  # Ensure `expr` is a string literal, a symbol, or an expression of the form
  # `pkg::fun` or `pkg:::fun`.
  if (rlang::is_call(expr)) {
    if (!identical(expr[[1]], quote(`::`)) ||
        !rlang::is_symbol(expr[[2]]) ||
        !rlang::is_symbol(expr[[3]])) {
      cli::cli_abort("Expected a symbol or a string literal, or an expression of the form `pkg::fun` or `pkg:::fun`.")
    }
    pkg <- as.character(expr[[2]])
    fun <- as.character(expr[[3]])
  } else if (!rlang::is_symbol(expr) && !rlang::is_string(expr)) {
    cli::cli_abort("Expected a symbol or a string literal, or an expression of the form `pkg::fun` or `pkg:::fun`.")
  }

  help_text <- get_help_text(fun, pkg)

  topic_str <- format(expr)

  tool_prompt <- readLines(system.file("tool_prompt.md", package = "elmer"), warn = FALSE)
  tool_prompt <- paste(tool_prompt, collapse = "\n")

  chat <- new_chat_openai(system_prompt = tool_prompt, model = model, echo = echo)
  chat$chat(paste0(
    "Function name: ", topic_str,
    "\n\nFunction documentation:\n\n", help_text
  ))
}
