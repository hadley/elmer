#' @include provider.R
#' @include types.R

method(as_json, list(Provider, TypeBasic)) <- function(provider, x) {
  list(type = x@type, description = x@description %||% "")
}

method(as_json, list(Provider, TypeEnum)) <- function(provider, x) {
  list(
    type = "string",
    description = x@description %||% "",
    enum = as.list(x@values)
  )
}

method(as_json, list(Provider, TypeObject)) <- function(provider, x) {
  names <- names2(x@properties)
  required <- map_lgl(x@properties, function(prop) prop@required)

  properties <- as_json(provider, x@properties)
  names(properties) <- names

  list(
    type = "object",
    description = x@description %||% "",
    properties = properties,
    required = as.list(names[required]),
    additionalProperties = x@additional_properties
  )
}

method(as_json, list(Provider, TypeArray)) <- function(provider, x) {
  list(
    type = "array",
    description = x@description %||% "",
    items = as_json(provider, x@items)
  )
}
