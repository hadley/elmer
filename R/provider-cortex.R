#' @include provider.R
#' @include content.R
#' @include turns.R
#' @include tools-def.R
NULL

#' Create a chatbot that speaks to the Snowflake Cortex Analyst
#'
#' @description
#' Chat with the LLM-powered [Snowflake Cortex
#' Analyst](https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-analyst).
#'
#' Unlike most comparable model APIs, Cortex does not take a system prompt.
#' Instead, the caller must provide a "semantic model" describing available
#' tables, their meaning, and verified queries that can be run against them as a
#' starting point. The semantic model can be passed as a YAML string or via
#' reference to an existing file in a Snowflake Stage.
#'
#' Note that Cortex does not support multi-turn, so it will not remember
#' previous messages. Nor does it support registering tools, and attempting to
#' do so will result in an error.
#'
#' By default we pick up on Snowflake connection parameters defined in the same
#' `connections.toml` file used by the [Python Connector for
#' Snowflake](https://docs.snowflake.com/en/developer-guide/python-connector/python-connector-connect)
#' and the [Snowflake
#' CLI](https://docs.snowflake.com/en/developer-guide/snowflake-cli/connecting/configure-connections),
#' though connection parameters can be passed manually to
#' [snowflakeauth::snowflake_connection()], too. Keep in mind that Cortex
#' itself only supports OAuth and key-pair authentication.
#'
#' @param model_spec A semantic model specification, or `NULL` when
#'   using `model_file` instead.
#' @param model_file Path to a semantic model file stored in a Snowflake Stage,
#'   or `NULL` when using `model_spec` instead.
#' @param ... Further arguments passed to [snowflakeauth::snowflake_connection()].
#' @param session A Shiny session object, when using viewer-based credentials on
#'   Posit Connect.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @examplesIf FALSE
#' # Authenticate with Snowflake using an existing connections.toml file.
#' chat <- chat_cortex(
#'   model_file = "@my_db.my_schema.my_stage/model.yaml"
#' )
#' chat$chat("What questions can I ask?")
#'
#' # Or pass connection parameters manually. For example, to use key-pair
#' # authentication:
#' chat <- chat_cortex(
#'   model_file = "@my_db.my_schema.my_stage/model.yaml",
#'   account = "myaccount",
#'   user = "me",
#'   private_key = "rsa_key.p8"
#' )
#' @export
chat_cortex <- function(model_spec = NULL,
                        model_file = NULL,
                        api_args = list(),
                        echo = c("none", "text", "all"),
                        ...,
                        session = NULL) {
  check_installed("snowflakeauth", "for Snowflake authentication")
  check_string(model_spec, allow_empty = FALSE, allow_null = TRUE)
  check_string(model_file, allow_empty = FALSE, allow_null = TRUE)
  check_exclusive(model_spec, model_file)
  echo <- check_echo(echo)
  check_shiny_session(session, allow_null = TRUE, call = call)

  connection <- snowflakeauth::snowflake_connection(..., .call = current_env())

  provider <- ProviderCortex(
    connection = connection,
    model_spec = model_spec,
    model_file = model_file,
    session = session,
    extra_args = api_args
  )

  Chat$new(provider = provider, turns = NULL, echo = echo)
}

ProviderCortex <- new_class(
  "ProviderCortex",
  parent = Provider,
  package = "elmer",
  constructor = function(connection,
                         model_spec = NULL,
                         model_file = NULL,
                         session = NULL,
                         extra_args = list()) {
    extra_args <- compact(list2(
      semantic_model = model_spec,
      semantic_model_file = model_file,
      !!!extra_args
    ))
    if (!is.null(session)) {
      # If viewer-based authentication is enabled, check whether we can actually
      # get credentials. If we can, then make sure the authenticator is OAuth.
      access_token <- connect_viewer_token(session, snowflake_url(connection))
      if (!is.null(access_token)) {
        connection$authenticator <- "oauth"
        connection$user <- "placeholder"
        connection$token <- access_token
      } else {
        session <- NULL
      }
    }
    new_object(
      Provider(base_url = snowflake_url(connection), extra_args = extra_args),
      connection = connection,
      session = session
    )
  },
  properties = list(
    connection = class_list,
    session = class_list | NULL,
    credentials = new_property(class_list, getter = function(self) {
      if (!is.null(self@session)) {
        # TODO: Right now we ask Connect for an up-to-date token before each
        # Cortex request. Instead, we should request a new token only when the
        # cached one has expired -- but right now there is no way to know when
        # this occurs.
        self@connection$token <- connect_viewer_token(
          self@session,
          snowflake_url(self@connection)
        )
      }
      snowflakeauth::snowflake_credentials(self@connection)
    }),
    extra_args = class_list
  )
)

snowflake_url <- function(connection) {
  paste0("https://", connection$account, ".snowflakecomputing.com")
}

# See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/reference/cortex-analyst
#      https://docs.snowflake.com/en/user-guide/snowflake-cortex/cortex-analyst/tutorials/tutorial-1#step-3-create-a-streamlit-app-to-talk-to-your-data-through-cortex-analyst
method(chat_request, ProviderCortex) <- function(provider,
                                                 stream = TRUE,
                                                 turns = list(),
                                                 tools = list(),
                                                 spec = NULL,
                                                 extra_args = list()) {
  if (length(tools) != 0) {
    cli::cli_abort("Tools are not supported by Cortex.")
  }
  if (!is.null(spec) != 0) {
    cli::cli_abort("Structured data extraction is not supported by Cortex.")
  }

  req <- request(provider@base_url)
  req <- req_url_path_append(req, "/api/v2/cortex/analyst/message")
  req <- httr2::req_headers(req,
    !!!provider@credentials, .redact = "Authorization"
  )
  req <- req_retry(req, max_tries = 2)
  req <- req_timeout(req, 60)

  # Snowflake doesn't document the error response format for Cortex Analyst at
  # this time, but empirically errors look like the following:
  #
  # {
  #  "message" : "'@stage' is an invalid stage file URL (correct format: '@my_db.my_schema.my_stage/path/to/file.yaml')",
  #  "code" : null,
  #  "error_code" : "392706",
  #  "request_id" : "929dd0e5-a582-458c-a9d2-c1aed531bb66"
  # }
  req <- req_error(req, body = function(resp) resp_body_json(resp)$message)

  # Snowflake uses the User Agent header to identify "parter applications",
  # so identify requests as coming from "r_elmer" (unless an explicit
  # partner application is set via the ambient SF_PARTNER environment
  # variable).
  req <- req_user_agent(
    req, paste0("r_elmer/", utils::packageVersion("elmer"))
  )
  if (nchar(Sys.getenv("SF_PARTNER")) != 0) {
    req <- req_user_agent(req, Sys.getenv("SF_PARTNER"))
  }

  # Cortex does not yet support multi-turn chats.
  turns <- tail(turns, n = 1)
  messages <- as_json(provider, turns)
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  data <- compact(list2(messages = messages, stream = stream, !!!extra_args))
  req <- req_body_json(req, data)

  req
}

# Cortex -> elmer --------------------------------------------------------------

method(stream_parse, ProviderCortex) <- function(provider, event) {
  # While undocumented, Cortex seems to mostly follow OpenAI API conventions for
  # streaming, although Cortex uses a specific JSON payload rather than in-band
  # signalling that the stream has ended.
  if (identical(event$data, '{"status":"done","status_message":"Done"}')) {
    NULL
  } else {
    jsonlite::parse_json(event$data)
  }
}

method(stream_text, ProviderCortex) <- function(provider, event) {
  if (!is.null(event$status_message)) {
    # Skip status messages like "Interpreting question" or "Generating
    # suggestions".
    ""
  } else if (!is.null(event$text_delta)) {
    event$text_delta
  } else if (!is.null(event$statement_delta)) {
    # Emit a Markdown-formatted code block for SQL queries.
    paste0("\n\n```sql\n", event$statement_delta, "\n```")
  } else if (!is.null(event$suggestions_delta)) {
    # Emit a Markdown-formatted "Suggestions" section.
    if (event$suggestions_delta$index == 0) {
      paste0(
        "\n\n#### Suggestions\n\n- ",
        event$suggestions_delta$suggestion_delta
      )
    } else {
      paste0("\n- ", event$suggestions_delta$suggestion_delta)
    }
  } else {
    cli::cli_abort(
      "Unknown chunk type {.str {event$type}}.", .internal = TRUE
    )
  }
}

method(stream_merge_chunks, ProviderCortex) <- function(provider, result, chunk) {
  if (!is.null(chunk$status)) {
    # Skip status messages.
    result
  } else if (is.null(result)) {
    list(cortex_chunk_to_message(chunk))
  } else {
    elt <- cortex_chunk_to_message(chunk)
    idx <- chunk$index + 1
    # This is a new chunk, we don't need to merge.
    if (length(result) < idx) {
      result[[idx]] <- elt
      return(result)
    }
    # Only suggestion-type chunks are emitted piecemeal.
    if (identical(chunk$type, "suggestions")) {
      result[[idx]]$suggestions <- c(
        result[[idx]]$suggestions, elt$suggestions
      )
    } else {
      cli::cli_abort(
        "Unmergeable chunk type {.str {chunk$type}}.", .internal = TRUE
      )
    }
    result
  }
}

cortex_chunk_to_message <- function(x) {
  if (x$type == "text") {
    list(type = x$type, text = x$text_delta)
  } else if (x$type == "sql") {
    list(type = x$type, statement = x$statement_delta)
  } else if (x$type == "suggestions") {
    list(
      type = x$type,
      suggestions = list(x$suggestions_delta$suggestion_delta)
    )
  } else {
    cli::cli_abort(
      "Unknown chunk type {.str {x$type}}.", .internal = TRUE
    )
  }
}

method(stream_turn, ProviderCortex) <- function(provider, result, has_spec = FALSE) {
  # We somehow lose the role when streaming, so add it back.
  cortex_message_to_turn(list(role = "assistant", content = result))
}

method(value_turn, ProviderCortex) <- function(provider, result, has_spec = FALSE) {
  cortex_message_to_turn(result$message)
}

# elmer -> Cortex --------------------------------------------------------------

# Cortex supports not only "text" content, but also bespoke "suggestions" and
# "sql" types.

method(as_json, list(ProviderCortex, Turn)) <- function(provider, x) {
  role <- x@role
  if (role == "assistant") {
    role <- "analyst"
  }
  list(
    role = role,
    content = as_json(provider, x@contents)
  )
}

method(as_json, list(ProviderCortex, ContentText)) <- function(provider, x) {
  list(type = "text", text = x@text)
}

ContentSuggestions <- new_class(
  "ContentSuggestions",
  parent = Content,
  properties = list(suggestions = class_character),
  package = "elmer"
)

method(as_json, list(ProviderCortex, ContentSuggestions)) <- function(provider, x) {
  list(type = "suggestions", suggestions = as.list(x@suggestions))
}

method(contents_text, ContentSuggestions) <- function(content) {
  # Emit a Markdown-formatted "Suggestions" section as the textual
  # representation.
  paste0(
    c("\n\n#### Suggestions\n", paste0("- ", content@suggestions)),
    collapse = "\n"
  )
}

method(format, ContentSuggestions) <- function(x, ...) {
  items <- x@suggestions
  names(items) <- rep("*", times = length(items))
  paste0(
    c(
      cli::format_inline("{.strong Suggestions:}"),
      cli::format_bullets_raw(items)
    ),
    collapse = "\n"
  )
}

ContentSql <- new_class(
  "ContentSql",
  parent = Content,
  properties = list(statement = prop_string()),
  package = "elmer"
)

method(as_json, list(ProviderCortex, ContentSql)) <- function(provider, x) {
  list(type = "sql", statement = x@statement)
}

method(contents_text, ContentSql) <- function(content) {
  # Emit a Markdown-formatted SQL code block as the textual representation.
  paste0("\n\n```sql\n", content@statement, "\n```")
}

method(format, ContentSql) <- function(x, ...) {
  cli::format_inline("{.strong SQL:} {.code {x@statement}}")
}

cortex_message_to_turn <- function(message, error_call = caller_env()) {
  role <- message$role
  if (role == "analyst") {
    role <- "assistant"
  }
  Turn(
    role = role,
    contents = lapply(message$content, function(x) {
      if (x$type == "text") {
        if (!has_name(x, "text")) {
          cli::cli_abort(
            "'text'-type content must have a 'text' field.", call = error_call
          )
        }
        ContentText(x$text)
      } else if (identical(x$type, "suggestions")) {
        if (!has_name(x, "suggestions")) {
          cli::cli_abort(
            "'suggestions'-type content must have a 'suggestions' field.",
            call = error_call
          )
        }
        ContentSuggestions(unlist(x$suggestions))
      } else if (identical(x$type, "sql")) {
        if (!has_name(x, "statement")) {
          cli::cli_abort(
            "'sql'-type content must have a 'statement' field.",
            call = error_call
          )
        }
        ContentSql(x$statement)
      } else {
        cli::cli_abort(
          "Unknown content type {.str {x$type}} in response.", .internal = TRUE
        )
      }
    })
  )
}
