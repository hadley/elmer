# Given a promise-yielding expression, loop until it resolves or rejects.
# DON'T USE THIS TECHNIQUE IN SHINY, PLUMBER, OR HTTPUV CONTEXTS.
sync <- function(expr) {
  p <- force(expr)

  done <- FALSE
  success <- NULL
  error <- NULL

  promises::then(p,
    function(result) {
      success <<- result
      done <<- TRUE
    },
    function(err) {
      error <<- err
      done <<- TRUE
    }
  )

  while (!done) {
    later::run_now(0.25)
  }
  if (!is.null(error)) {
    stop(error)
  } else {
    success
  }
}
