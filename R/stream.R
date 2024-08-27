# Want callback for each message too?
req_perform_chat <- function(req,
                             messages,
                             stream = identity) {

  force(messages)
  force(stream)
  results <- list()

  callback <- function(bytes) {
    sse_text <- rawToChar(bytes)
    sse_messages <- strsplit(sse_text, "\n\n")[[1]]
    sse_lines <- strsplit(sse_messages, "\n")

    # shouldn't have messages with multiple lines > 1
    if (any(lengths(sse_lines) > 1)) {
      abort("Malformed stream", .internal = TRUE)
    }
    lines <- unlist(sse_lines)
    is_data <- grepl("^data: ", lines)

    # should only be data messages
    if (!all(is_data)) {
      abort("Malformed stream", .internal = TRUE)
    }
    contents <- sub("^data: ", "", lines)

    is_done <- any(contents == "[DONE]")
    contents <- contents[contents != "[DONE]"]
    jsons <- lapply(contents, jsonlite::parse_json)

    for (json in jsons) {
      results <<- merge_dicts(results, json)
      stream(json$choices[[1]]$delta)
    }

    TRUE
  }
  resp <- httr2::req_perform_stream(req, callback, wait_for = 0, round = "sse")

  result_message1 <- results$choices[[1]]$delta # $message if not streaming
  messages <- c(messages, list(result_message1), call_tools(result_message1))
  messages
}
