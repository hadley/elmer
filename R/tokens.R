tokens_log <- function(name, tokens) {
  if (is.null(the$tokens)) {
    the$tokens <- list()
  }
  if (is.null(the$tokens[[name]])) {
    the$tokens[[name]] <- c(0, 0)
  }

  tokens[is.na(tokens)] <- 0
  the$tokens[[name]] <- the$tokens[[name]] + tokens
  invisible()
}

tokens_reset <- function() {
  the$tokens <- NULL
  invisible()
}

#' Report on token usage in the current session
#'
#' Call this function to find out the cumulative number of tokens that you
#' have sent and recieved in the current session.
#'
#' @export
#' @return A data frame
#' @examples
#' token_usage()
token_usage <- function() {
  if (is.null(the$tokens)) {
    cli::cli_inform(c(x = "No recorded usage in this session"))
    return(invisible(
      data.frame(name = character(), input = numeric(), output = numeric())
    ))
  }

  rows <- map2(names(the$tokens), the$tokens, function(name, tokens) {
    data.frame(name = name, input = tokens[[1]], output = tokens[[2]])
  })
  do.call("rbind", rows)
}
