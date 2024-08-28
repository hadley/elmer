# Want callback for each message too?
req_perform_chat <- function(req, messages) {

  force(messages)
  results <- list()
  resp <- httr2::req_perform_connection(req)

  repeat({
    event <- httr2::resp_stream_sse(resp)
    if (is.null(event) || event$data == "[DONE]") {
      break
    }
    json <- jsonlite::parse_json(event$data)
    cat(json$choices[[1]]$delta$content)

    results <<- merge_dicts(results, json)
  })


  result_message1 <- results$choices[[1]]$delta # $message if not streaming
  messages <- c(messages, list(result_message1), call_tools(result_message1))
  messages
}
