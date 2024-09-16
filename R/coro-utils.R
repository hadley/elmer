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

generators <- new_environment()
generators$cur_id <- 1L

new_id <- function() {
  generators$cur_id <- generators$cur_id + 1L
  as.character(generators$cur_id)
}

# Decorator for anonymous functions; the return value is intended to be used as
# an R6 method. Unlike regular R6 methods, the decorated function must have
# `self` as the first argument (which will be automatically passed in by the
# decorator). If necessary we can also provide access to `private` in the same
# way.
R6_decorate <- function(wrapper, func, print = FALSE) {
  wrapper <- enexpr(wrapper)
  fn <- enexpr(func)

  arg_names <- names(formals(func))
  if (length(arg_names) < 2) {
    cli::cli_abort("Function must have at least two arguments.", .internal = TRUE)
  } else if (arg_names[[1]] != "self") {
    cli::cli_abort("First argument must be {.arg self}.", .internal = TRUE)
  } else if (arg_names[[2]] != "private") {
    cli::cli_abort("Second argument must be {.arg private}.", .internal = TRUE)
  }

  args_def <- formals(func)[-(1:2)]
  args_call <- lapply(set_names(names(args_def)), as.symbol)

  id <- new_id()
  generators[[id]] <- inject((!!wrapper)(!!fn), parent.frame())

  # Supress R CMD check note
  self <- private <- NULL

  # Must use elmer::: because the lexical environment of this function is
  # about to get wrecked by R6
  gen_method <- new_function(args_def,
    expr(elmer:::generators[[!!id]](self, private, !!!args_call))
  )
  if (print) {
    print(gen_method, internals = TRUE)
  }
  gen_method
}
