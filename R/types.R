Type <- new_class(
  "Type",
  properties = list(
    description = prop_string(allow_null = TRUE),
    required = prop_bool()
  )
)
TypeBasic <- new_class(
  "TypeBasic",
  Type,
  properties = list(
    type = prop_string()
  )
)
TypeEnum <- new_class(
  "TypeEnum",
  Type,
  properties = list(
    values = class_character
  )
)
TypeArray <- new_class(
  "TypeArray",
  Type,
  properties = list(
    items = Type
  )
)
TypeObject <- new_class(
  "TypeObject",
  Type,
  properties = list(
    properties = prop_list_of(Type, names = "all"),
    additional_properties = prop_bool()
  )
)

#' Type specifications
#'
#' @description
#' These functions specify object types in a way that chatbots understand and
#' are used for tool calling and structured data extraction. Their names are
#' based on the [JSON schema](https://json-schema.org), which is what the APIs
#' expect behind the scenes. The translation from R concepts to these types is
#' fairly straightforward.
#'
#' * `type_boolean()`, `type_integer()`, `type_number()`, and `type_string()`
#'   each represent scalars. These are equivalent to length-1 logical,
#'   integer, double, and character vectors (respectively).
#'
#' * `type_enum()` is equivalent to a length-1 factor; it is a string that can
#'   only take the specified values.
#'
#' * `type_array()` is equivalent to a vector in R. You can use it to represent
#'   an atomic vector: e.g. `type_array(items = type_boolean())` is equivalent
#'   to a logical vector and `type_array(items = type_string())` is equivalent
#'   to a character vector). You can also use it to represent a list of more
#'   complicated types where every element is the same type (R has no base
#'   equivalent to this), e.g. `type_array(items = type_array(items = type_string()))`
#'   represents a list of character vectors.
#'
#' * `type_object()` is equivalent to a named list in R, but where every element
#'   must have the specified type. For example,
#'   `type_object(a = type_string(), b = type_array(type_integer()))` is
#'   equivalent to a list with an element called `a` that is a string and
#'   an element called `b` that is an integer vector.
#'
#' @param description,.description The purpose of the component. This is
#'   used by the LLM to determine what values to pass to the tool or what
#'   values to extract in the structured data, so the more detail that you can
#'   provide here, the better.
#' @param required,.required Is the component required? If `FALSE`, and the
#'   component does not exist in the data, the LLM may hallucinate a value.
#'   Only applies when the element is nested inside of a `type_object()`.
#' @export
#' @examples
#' # An integer vector
#' type_array(items = type_integer())
#'
#' # The closest equivalent to a data frame is an array of objects
#' type_array(items = type_object(
#'    x = type_boolean(),
#'    y = type_string(),
#'    z = type_number()
#' ))
#'
#' # There's no specific type for dates, but you use a string with the
#' # requested format in the description (it's not gauranteed that you'll
#' # get this format back, but you should most of the time)
#' type_string("The creation date, in YYYY-MM-DD format.")
#' type_string("The update date, in dd/mm/yyyy format.")
type_boolean <- function(description = NULL, required = TRUE) {
  TypeBasic(type = "boolean", description = description, required = required)
}
#' @export
#' @rdname type_boolean
type_integer <- function(description = NULL, required = TRUE) {
  TypeBasic(type = "integer", description = description, required = required)
}
#' @export
#' @rdname type_boolean
type_number <- function(description = NULL, required = TRUE) {
  TypeBasic(type = "number", description = description, required = required)
}
#' @export
#' @rdname type_boolean
type_string <- function(description = NULL, required = TRUE) {
  TypeBasic(type = "string", description = description, required = required)
}

#' @param values Character vector of permitted values.
#' @export
#' @rdname type_boolean
type_enum <- function(description = NULL, values, required = TRUE) {
  TypeEnum(values = values, description = description, required = required)
}

#' @param item The type of the array items. Can be created by any of the
#'   `type_` function.
#' @export
#' @rdname type_boolean
type_array <- function(description = NULL, items, required = TRUE) {
  TypeArray(items = items, description = description, required = required)
}

#' @param ... Name-type pairs defineing the components that the object must
#'   possess.
#' @param .additional_properties Can the object have arbitrary additional
#'   properties that are not explicitly listed? Only supported by Claude.
#' @export
#' @rdname type_boolean
type_object <- function(.description = NULL,
                        ...,
                        .required = TRUE,
                        .additional_properties = FALSE) {
  TypeObject(
    properties = list2(...),
    description = .description,
    required = .required,
    additional_properties = .additional_properties
  )
}

# JSON schema ------------------------------------------------------------

as_json_schema <- new_generic("as_json_schema", c("provider", "x"))

method(as_json_schema, list(Provider, TypeBasic)) <- function(provider, x) {
  list(type = x@type, description = x@description %||% "")
}

method(as_json_schema, list(Provider, TypeEnum)) <- function(provider, x) {
  list(
    type = "string",
    description = x@description %||% "",
    enum = as.list(x@values)
  )
}

method(as_json_schema, list(Provider, TypeObject)) <- function(provider, x) {
  names <- names2(x@properties)
  required <- map_lgl(x@properties, function(prop) prop@required)

  properties <- lapply(x@properties, as_json_schema, provider = provider)
  names(properties) <- names

  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names[required]),
    additionalProperties = x@additional_properties
  )
}

method(as_json_schema, list(ProviderOpenAI, TypeObject)) <- function(provider, x) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for OpenAI.")
  }

  names <- names2(x@properties)
  properties <- lapply(x@properties, function(x) {
    out <- as_json_schema(provider, x)
    if (!x@required) {
      out$type <- c(out$type, "null")
    }
    out
  })

  names(properties) <- names

  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names),
    additionalProperties = x@additional_properties
  )
}

method(as_json_schema, list(ProviderGemini, TypeObject)) <- function(provider, x) {
  if (x@additional_properties) {
    cli::cli_abort("{.arg .additional_properties} not supported for Gemini.")
  }

  if (length(x@properties) == 0) {
    return(list())
  }

  names <- names2(x@properties)
  required <- map_lgl(x@properties, function(prop) prop@required)

  properties <- lapply(x@properties, as_json_schema, provider = provider)
  names(properties) <- names

  compact(list(
    type = "object",
    description = x@description,
    properties = properties,
    required = as.list(names[required])
  ))
}


method(as_json_schema, list(Provider, TypeArray)) <- function(provider, x) {
  list(
    type = "array",
    description = x@description %||% "",
    items = as_json_schema(provider, x@items)
  )
}
