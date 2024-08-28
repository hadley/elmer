

tool_def <- function(name, description, arguments, strict = TRUE) {
  arg_names <- map_chr(arguments, "[[", "name")
  arg_required <- map_lgl(arguments, "[[", "required")

  properties <- lapply(arguments, function(x) x[c("type", "description")])
  names(properties) <- arg_names

  list(
    type = "function",
    "function" = list(
      name = name,
      description = description,
      strict = strict,
      parameters = list(
        type = "object",
        properties = properties,
        required = arg_names[arg_required],
        additionalProperties = FALSE
      )
    )
  )
}

tool_arg <- function(name, type, description, required = TRUE) {
  list(
    name = name,
    type = type,
    description = description,
    required = required
  )
}

rnorm <- tool_def(
  "rnorm",
  "Drawn numbers from a random normal distribution",
  list(
    tool_arg(
      "n",
      type = "integer",
      description = "The number of observations. Must be a positive integer."
    ),
    tool_arg(
      "mean",
      type = "number",
      description = "The mean value of the distribution."
    ),
    tool_arg(
      "sd",
      type = "number",
      description = "The standard deviation of the distribution. Must be a non-negative number."
    )
  )
)
