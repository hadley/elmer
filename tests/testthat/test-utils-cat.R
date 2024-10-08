# Execute a function that sinks text to the console and capture the output.
capture_sink <- function(prefix, func, width = 64) {
  con <- file(open = "w+b")
  on.exit(close(con))
  sink <- cat_word_wrap(con, prefix = prefix, width = width)
  func(sink)
  sink(coro::exhausted())
  len <- seek(con, 0)
  readChar(con, len)
}

sink_test_equal <- function(prefix, expected, func, width = 64) {
  value <- capture_sink(prefix, func, width = width)
  rlang::inject(expect_equal(value, {{expected}}))
}

test_that("wrap + prefix interactions", {
  sink_test_equal(prefix = ">>>", expected = ">>>\n", function(sink) {
    sink("\n")
  }, width = 12)

  sink_test_equal(prefix = "> ", expected = "> a\n> b\n", function(sink) {
    sink("a\n")
    sink("b\n")
  }, width = 12)

  sink_test_equal(
    prefix = "> ",
    expected = "> this_is_a_very_long_string_that_nevertheless_should_not_wrap\n",
    function(sink) {
      sink("this_is_a_very_long_string_that_nevertheless_should_not_wrap\n")
    },
    width = 12
  )

  # Just barely fits (11 chars wide)
  sink_test_equal(
    prefix = "12",
    expected = "12345678 AB\n",
    function(sink) {
      sink("345678 AB\n")
    },
    width = 12
  )

  # Doesn't fit, but can't wrap
  sink_test_equal(
    prefix = "12",
    expected = "123456789ABCD\n",
    function(sink) {
      sink("3456789ABCD\n")
    },
    width = 12
  )

  # Doesn't fit, but wraps
  sink_test_equal(
    prefix = "> ",
    expected = "> 345678 \n> ABCD\n",
    function(sink) {
      sink("345678 ABCD\n")
    },
    width = 12
  )
})

test_that("more realistic wrap examples", {
  for (prefix in c("?> ", "")) {
    for (width in c(70, 10000)) {
      rlang::inject({
        expect_snapshot(cat(capture_sink(prefix = !!prefix, width = !!width, function(sink) {
          sink("It was the best of times, it was the worst of times, it was the age ")
          sink("of wisdom, it was the age of foolishness, it was the epoch of belief")
          sink(", it was the epoch of incredulity, it was the season of Light, it wa")
          sink("s the season of Darkness, it was the spring of hope, it was the wint")
          sink("er of despair.")
        })))
      })
    }
  }

  expect_snapshot(cat(capture_sink(prefix = "Dickens> ", function(sink) {
    sink("It was the best of times,\nit was the worst of times,\n")
    sink("it was the age ")
    sink("of wisdom, it was the age of ")
    sink("foolishness,\nit was the epoch of belief")
    sink(", it was the epoch of incredulity, ")
    sink(paste(rep_len(" ", 200), collapse = ""))
    sink("it was the season of Light, it wa")
    sink("s the season of Darkness,\n\n\nit was the spring of hope,")
    sink(" it was the wint")
    sink("er of despair.")
  })))
})
