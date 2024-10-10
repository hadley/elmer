
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

#' Type defintions
#'
#' @description
#' These functions define object types in a way that chatbots understand and
#' are used for tool calling and structured data extraction. There names are
#' based on the [JSON schema](https://json-schema.org), which is what the APIs
#' expected behind the scenes, but it's fairly straightforward to translated
#' from what you know about R.
#'
#' @param description,.description The purpose of the component. Generally,
#'   the more information that you can provide here, the better.
#' @param required,.required Is the component required?
#' @export
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

as_json_schema <- new_generic("as_json_schema", "x")

method(as_json_schema, TypeBasic) <- function(x) {
  list(type = x@type, description = x@description %||% "")
}

method(as_json_schema, TypeEnum) <- function(x) {
  list(
    type = "string",
    description = x@description %||% "",
    enum = as.list(x@values)
  )
}

method(as_json_schema, TypeObject) <- function(x) {
  names <- names2(x@properties)
  required <- map_lgl(x@properties, function(prop) prop@required)

  properties <- lapply(x@properties, as_json_schema)
  names(properties) <- names

  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names[required]),
    additionalProperties = x@additional_properties
  )
}

method(as_json_schema, TypeArray) <- function(x) {
  list(
    type = "array",
    description = x@description %||% "",
    items = as_json_schema(x@items)
  )
}
