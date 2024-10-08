cat_line <- function(..., prefix = "") {
  lines <- paste0(..., collapse = "\n")
  sink <- cat_word_wrap(prefix = prefix)
  sink(lines)
  if (!endsWith(lines, "\n")) {
    sink("\n")
  }
  sink(coro::exhausted())
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

# A sink is a function that takes a string and returns `invisible()`. The
# `prepare_sink` function takes a raw function that can take a string as its
# first argument, and returns whatever, and returns a wrapper sink function. It
# also primes generators.
prepare_sink <- function(raw_sink_func, ...) {
  if (inherits(raw_sink_func, "coro_generator_instance")) {
    # Generators need to be primed before they can be used
    raw_sink_func(...)
  }

  function(str) {
    raw_sink_func(str, ...)
    invisible()
  }
}


# Sink decorators ---------------------------------------------------------
#
# A sink decorator is a sink that wraps another sink, modifying or coalescing or
# redacting or otherwise messing with the input before passing it to the target
# sink.
#
# We currently have two sink decorators:
# - sink_wordwrap: wraps a sink to perform word wrapping
# - sink_prefix: wraps a sink to prefix each line with a given string
#
# Creating a sink decorator is a two-step process:
# 1. Define a generator that takes a sink and some other arguments, and calls
#    `yield()` to get input and `sink()` to pass output to the target sink.
# 2. Define a wrapper function that takes the target sink and any other
#    arguments, and returns a sink that wraps the target sink with the generator
#    from step 1. It's important that the wrapper function calls `force()` on
#    all of its arguments, as the generator is not likely to trigger evaluation
#    until relatively late. It also needs to call `prepare_sink` to ensure that
#    the generator is primed before it's used.

sink_wordwrap <- function(sink, width) {
  force(sink)
  force(width)
  prepare_sink(sink_wordwrap_gen(sink, width))
}

sink_wordwrap_gen <- NULL
on_load(sink_wordwrap_gen <- coro::generator(function(sink, width) {
  buffer <- ""
  pos_cursor <- 1

  exhausted <- FALSE
  while (!exhausted) {
    input <- coro::yield()
    if (coro::is_exhausted(input)) {
      exhausted <- TRUE
      input <- ""
    }

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

      token_width <- nchar(token)
      is_space <- startsWith(token, " ")
      # Uncomment to render whitespace
      # token <- gsub(" ", "\u00B7", token)

      if (token == "\n") {
        # Always flush newlines immediately
        sink("\n")
        pos_cursor <- 1
      } else if (is_last_token && !exhausted) {
        # A trailing non-newline might not be complete; buffer it until we have
        # more text to process
        buffer <- token
      } else {
        # A regular token. See if it fits on the current line.
        soft_wrap <- pos_cursor + token_width > width

        if (soft_wrap && (pos_cursor != 1 || is_space)) {
          # It doesn't fit; wrap to the next line
          sink("\n")
          pos_cursor <- 1

          if (is_space) {
            # soft-wrapping due to spaces; skip rendering the spaces
            next
          }
        }

        # Render the token and update the cursor
        sink(token)
        pos_cursor <- pos_cursor + token_width
      }
    }
  }
}))

# Wrap a sink to prefix each line with a given string.
sink_prefix <- function(sink, prefix = "") {
  force(sink)
  force(prefix)
  prepare_sink(sink_prefix_gen(sink, prefix))
}

sink_prefix_gen <- NULL
on_load(sink_prefix_gen <- coro::generator(function(sink, prefix = "") {
  # When at line start, the next input will be prefixed with `prefix`. (But if
  # the next input never comes, we won't print the prefix.)
  at_line_start <- TRUE

  # In case the prefix itself contains characters that have special meaning when
  # used as a gsub replacement
  prefix_escaped <- gsub(perl = TRUE, "(\\\\|\\$)", "\\\\\\1", prefix)

  repeat {
    input <- coro::yield(invisible())
    if (coro::is_exhausted(input)) {
      break
    }
    if (nchar(input) == 0) {
      next
    }

    # We have non-empty input. If we're at the start of the line, it's time to
    # print a prefix.
    if (at_line_start) {
      sink(prefix)
      at_line_start <- FALSE
    }

    # Add the prefix after each newline in the input except the last one.
    transformed <- gsub(
      perl = TRUE,
      "\n(?!$)", # Matches \n that are NOT immediately followed by EOF
      paste0("\n", prefix_escaped),
      input
    )
    sink(transformed)

    # If the input ends with a newline, let the _next_ input be prefixed.
    if (endsWith(input, "\n")) {
      at_line_start <- TRUE
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
cat_word_wrap <- function(con = stdout(), prefix = "", width = cli::console_width()) {
  sink <- prepare_sink(cat, file = con, sep = "")

  if (nchar(prefix) > 0) {
    sink <- sink_prefix(sink, prefix)
  }

  sink <- sink_wordwrap(sink, width - nchar(prefix))

  sink
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
