# Utility functions for using coro together with R6. This is ordinarily not
# possible because R6 replaces a method's containing environment with its own,
# and coro generators need their containing environments.
#
# We solve this by storing pristine coro generator functions in a special
# environment, and create sacrificial wrapper functions to serve as methods;
# these wrappers do nothing but fetch the real generators and invoke them.
#
# TODO: Check with @hadley if it's safe to do this in a package, given that the
# R6 classes that depend on this will be instantiated at package build time; so
# the coro generator functions will be burned into the package .Rds file.

# So `R CMD check` doesn't get confused about these variables being used from
# methods
utils::globalVariables(c("self", "private"))

generators <- new.env()

# Decorator for anonymous functions; the return value is intended to be used as
# an R6 method. Unlike regular R6 methods, the decorated function must have
# `self` as the first argument (which will be automatically passed in by the
# decorator). If necessary we can also provide access to `private` in the same
# way.
generator_method <- function(func) {
  fn <- substitute(func)

  stopifnot(
    "generator methods must have `self` parameter" = identical(names(formals(func))[1], "self")
  )
  stopifnot(
    "generator methods must have `self` parameter" = identical(names(formals(func))[2], "private")
  )

  expr <- rlang::inject(
    base::quote(coro::generator(!!fn))
  )
  generator <- eval(expr, parent.frame())

  unique_id <- as.character(sample.int(99999999, 1))
  generators[[unique_id]] <- generator

  rlang::inject(
    function(...) {
      # Can't simply use `generators` because the lexical environment of this
      # function is about to get wrecked by R6
      getNamespace("elmer")[["generators"]][[!!unique_id]](self, private, ...)
    }
  )
}

# Same as generator_method, but for async logic
async_generator_method <- function(func, print = FALSE) {
  fn <- substitute(func)

  stopifnot(
    "generator methods must have `self` parameter" = identical(names(formals(func))[1], "self")
  )
  stopifnot(
    "generator methods must have `self` parameter" = identical(names(formals(func))[2], "private")
  )

  expr <- rlang::inject(
    base::quote(coro::async_generator(!!fn))
  )
  generator <- eval(expr, parent.frame())

  unique_id <- paste0("a", sample.int(99999999, 1))
  generators[[unique_id]] <- generator

  gen <- rlang::inject(
    function(...) {
      # Can't simply use `generators` because the lexical environment of this
      # function is about to get wrecked by R6
      getNamespace("elmer")[["generators"]][[!!unique_id]](self, private, ...)
    }
  )
  if (print) {
    print(gen, internals = TRUE)
  }
  gen
}
