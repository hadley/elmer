cat_line <- function(..., prefix = "") {
  lines <- paste0(..., collapse = "\n")
  lines <- paste0(prefix, gsub("\n", paste0("\n", prefix), lines, fixed = TRUE), "\n")
  cat(lines)
}

# Utility functions for performing word wrapping on streaming text. Our strategy
# for dealing with streaming is to buffer the most recent word, assuming that
# it's incomplete, and render it only when the next word break (space or
# newline) is encountered.


# Split a string into contiguous runs of non-spaces, " ", and newline.
#
# split_spaces("one  two    three") => c("one", "  ", "two", "    ", "three")
split_spaces <- function(text) {
  if (length(text) != 1 || !is.character(text)) {
    stop("Input must be a character vector of length 1")
  }

  pattern <- "([^ \\n]+| +|\\n)"
  result <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  result
}

cat_word_wrap_impl <- NULL
on_load(cat_word_wrap_impl <- coro::generator(function(con = stdout(), prefix = "") {
  CAT <- function(...) {
    cat(..., file = con, sep = "")
  }

  console_width <- cli::console_width()

  pos_cursor <- 1
  buffer <- ""

  repeat {
    input <- coro::yield()

    input <- paste0(buffer, input)
    buffer <- ""

    # Normalize whitespace, make our lives easier
    input <- gsub("\r\n", "\n", input)
    input <- gsub("\r", "\n", input)
    input <- gsub("\t", "    ", input)

    tokens <- split_spaces(input)
    for (i in seq_along(tokens)) {
      token <- tokens[[i]]
      is_last_token <- i == length(tokens)

      width <- nchar(token)
      is_space <- startsWith(token, " ")
      # Uncomment to render whitespace
      # token <- gsub(" ", "\u00B7", token)

      if (token == "\n") {
        # Always flush newlines immediately
        CAT("\n", prefix)
        pos_cursor <- 1 + nchar(prefix)
      } else if (is_last_token) {
        # A trailing non-newline might not be complete; buffer it until we have
        # more text to process
        buffer <- token
      } else {
        if (pos_cursor == 1) {
          CAT(prefix)
          pos_cursor <- 1 + nchar(prefix)
        }

        # A regular token. See if it fits on the current line.
        soft_wrap <- pos_cursor + width > console_width

        if (soft_wrap) {
          # It doesn't fit; wrap to the next line
          CAT("\n", prefix)
          pos_cursor <- 1 + nchar(prefix)

          if (is_space) {
            # soft-wrapping due to spaces; skip rendering the spaces
            next
          }
        }

        # Render the token and update the cursor
        CAT(token)
        pos_cursor <- pos_cursor + width
      }
    }
  }
}))

# cat_word_wrap() is a function that returns a function that can be used to
# print text to the console with word wrapping. It uses the COLUMNS environment
# variable to work well from the terminal, and getWidth("options") to work well
# in RStudio.
#
# Each call to cat_word_wrap() (i.e. the creation of a catter) assumes that the
# current line of the given connection is empty. Each catter will assume it has
# sole possession of the connection (i.e. it ignores other code writing directly
# to its underlying connection).
#
# The catter buffers the last word of a line, assuming that it's incomplete. To
# ensure that the buffer gets flushed, cat a "\n" character.
cat_word_wrap <- function(con = stdout(), prefix = "") {
  cat_impl <- cat_word_wrap_impl(con, prefix)
  cat_impl("")

  function(str) {
    cat_impl(str)
    invisible(NULL)
  }
}

emitter <- function(echo, prefix) {
  if (echo == "text") {
    cat_word_wrap()
  } else if (echo == "all") {
    cat_word_wrap(prefix = "< ")
  } else {
    function(...) invisible()
  }
}
