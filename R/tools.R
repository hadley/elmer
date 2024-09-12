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

#' @export
create_tool_metadata <- function(symbol) {
  sym <- rlang::ensym(symbol)

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

  help_files <- rlang::inject(`?`(!!sym))
  if (length(help_files) == 0) {
    cli::cli_abort("No help files found")
  }

  # Has side effect of setting file_contents
  print(help_files)

  help_text <- paste(file_contents, collapse = "\n")

  tool_prompt <- readLines(system.file("tool_prompt.md", package = "elmer"), warn = FALSE)
  tool_prompt <- paste(tool_prompt, collapse = "\n")

  chat <- new_chat_openai(system_prompt = tool_prompt, echo = TRUE)
  chat$chat(paste0("Function name: ", as.character(sym), "\n\nFunction documentation:\n\n", help_text))
}
