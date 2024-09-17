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
utils::globalVariables(c("self", "private", "generator_env", "exits"))

generators <- new.env()

# Decorator for anonymous functions; the return value is intended to be used as
# an R6 method. Unlike regular R6 methods, the decorated function must have
# `self` as the first argument (which will be automatically passed in by the
# decorator). If necessary we can also provide access to `private` in the same
# way.
generator_method <- function(func) {
  fn <- rlang::enexpr(func)

  stopifnot(
    "generator methods must have `self` parameter" = identical(names(formals(func))[1], "self")
  )
  stopifnot(
    "generator methods must have `private` parameter" = identical(names(formals(func))[2], "private")
  )

  deferred_method_transform(fn, coro::generator, parent.frame())
}

# Same as generator_method, but for async logic
async_generator_method <- function(func) {
  fn <- rlang::enexpr(func)

  stopifnot(
    "async generator methods must have `self` parameter" = identical(names(formals(func))[1], "self")
  )
  stopifnot(
    "async generator methods must have `private` parameter" = identical(names(formals(func))[2], "private")
  )

  deferred_method_transform(fn, coro::async_generator, parent.frame())
}

# Takes a quoted function expression and a transformer function, and returns a
# function that will _lazily_ transform the lambda function using `transformer`
# upon first call. This is necessary because the transformation needs to be done
# not at package build time, but after package load time.
#
# Elsewhere in elmer, we use rlang::on_load to defer the transformation of
# generators until after package load time. We can't do that for R6 methods
# because nesting R6 class definitions inside of rlang::on_load causes roxygen2
# to get confused.
deferred_method_transform <- function(lambda_expr, transformer, eval_env) {
  tr <- rlang::enexpr(transformer)
  force(eval_env)

  unique_id <- paste0("a", sample.int(99999999, 1))

  delayedAssign(unique_id, {
    expr <- rlang::inject(
      base::quote((!!tr)(!!lambda_expr))
    )
    generator <- eval(expr, eval_env)
    generator
  }, assign.env = generators)

  rlang::inject(
    function(...) {
      # Can't simply use `generators` because the lexical environment of this
      # function is about to get wrecked by R6
      getNamespace("elmer")[["generators"]][[!!unique_id]](self, private, ...)
    }
  )
}
