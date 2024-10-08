#' A ChatBot Provider
#'
#' A provider captures the details of one chatbot service/API. To add support
#' for a new backend, you will need to subclass `Provider` (adding any
#' additional fields that your provider needs) and then implement the
#' various generics that control the behaviour of each provider.
#'
#' @export
#' @param base_url The base URL for the API.
#' @param api_key The api key used for authentication.
#' @param extra_args Arbitrary extra arguments included in the request body.
Provider <- new_class(
  "Provider",
  package = "elmer",
  properties = list(
    base_url = prop_string(),
    api_key = prop_string(),
    extra_args = class_list
  )
)

# Create a request------------------------------------

chat_request <- new_generic("chat_request", "provider",
  function(provider, stream = TRUE, turns = list(), tools = list(), extra_args = list()) {
    S7_dispatch()
  })

# Extract data from streaming results ------------------------------------

stream_parse <- new_generic("stream_parse", "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
stream_text <- new_generic("stream_text", "provider",
  function(provider, event) {
    S7_dispatch()
  }
)
stream_merge_chunks <- new_generic("stream_merge_chunks", "provider",
  function(provider, result, chunk) {
    S7_dispatch()
  }
)
stream_turn <- new_generic("stream_turn", "provider",
  function(provider, result) {
    S7_dispatch()
  }
)

# Extract data from non-streaming results --------------------------------------

value_turn <- new_generic("value_turn", "provider",
  function(provider, result) {
    S7_dispatch()
  }
)
