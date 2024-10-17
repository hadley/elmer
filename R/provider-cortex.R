#' @include content.R
#' @include provider.R
#' @include turns.R
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
#' @param account A Snowflake [account identifier](https://docs.snowflake.com/en/user-guide/admin-account-identifier),
#'   e.g. `"testorg-test_account"`.
#' @param credentials A list of authentication headers to pass into
#'   [`httr2::req_headers()`] or a function that returns them when passed
#'   `account` as a parameter. The default [`cortex_credentials()`] function
#'   picks up ambient Snowflake OAuth and key-pair authentication credentials
#'   and handles refreshing them automatically.
#' @param model_spec A semantic model specification, or `NULL` when
#'   using `model_file` instead.
#' @param model_file Path to a semantic model file stored in a Snowflake Stage,
#'   or `NULL` when using `model_spec` instead.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @family chatbots
#' @examplesIf elmer:::cortex_credentials_exist()
#' chat <- chat_cortex(
#'   model_file = "@my_db.my_schema.my_stage/model.yaml"
#' )
#' chat$chat("What questions can I ask?")
#' @export
chat_cortex <- function(account = Sys.getenv("SNOWFLAKE_ACCOUNT"),
                        credentials = cortex_credentials,
                        model_spec = NULL,
                        model_file = NULL,
                        api_args = list(),
                        echo = c("none", "text", "all")) {
  check_string(account, allow_empty = FALSE)
  check_string(model_spec, allow_empty = FALSE, allow_null = TRUE)
  check_string(model_file, allow_empty = FALSE, allow_null = TRUE)
  check_exclusive(model_spec, model_file)
  echo <- check_echo(echo)

  if (is_list(credentials)) {
    static_credentials <- force(credentials)
    credentials <- function(account) static_credentials
  }
  check_function(credentials)

  provider <- ProviderCortex(
    account = account,
    credentials = credentials,
    model_spec = model_spec,
    model_file = model_file,
    extra_args = api_args
  )

  Chat$new(provider = provider, turns = NULL, echo = echo)
}

ProviderCortex <- new_class(
  "ProviderCortex",
  parent = Provider,
  package = "elmer",
  constructor = function(account, credentials, model_spec = NULL,
                         model_file = NULL, extra_args = list()) {
    base_url <- paste0("https://", account, ".snowflakecomputing.com")
    extra_args <- compact(list2(
      semantic_model = model_spec,
      semantic_model_file = model_file,
      !!!extra_args
    ))
    new_object(
      Provider(base_url = base_url, extra_args = extra_args),
      account = account,
      credentials = credentials
    )
  },
  properties = list(
    account = prop_string(),
    credentials = class_function,
    extra_args = class_list
  )
)

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
    !!!provider@credentials(provider@account), .redact = "Authorization"
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
  messages <- lapply(turns, cortex_message)
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  data <- compact(list2(messages = messages, stream = stream, !!!extra_args))
  req <- req_body_json(req, data)

  req
}

# Streaming support ------------------------------------------------------------

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
    list(type = x$type, statement = x$statement)
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

# Non-streaming support --------------------------------------------------------

method(value_turn, ProviderCortex) <- function(provider, result, has_spec = FALSE) {
  cortex_message_to_turn(result$message)
}

# Conversions ------------------------------------------------------------------

# Cortex supports not only "text" content, but also bespoke "suggestions" and
# "sql" types.

cortex_message <- new_generic("cortex_message", "x")

method(cortex_message, Turn) <- function(x) {
  role <- x@role
  if (role == "assistant") {
    role <- "analyst"
  }
  list(
    role = role,
    content = lapply(x@contents, cortex_message)
  )
}

method(cortex_message, ContentText) <- function(x) {
  list(type = "text", text = x@text)
}

ContentSuggestions <- new_class(
  "ContentSuggestions",
  parent = Content,
  properties = list(suggestions = class_character),
  package = "elmer"
)

method(cortex_message, ContentSuggestions) <- function(x) {
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

method(cortex_message, ContentSql) <- function(x) {
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

# Credential handling ----------------------------------------------------------

cortex_credentials_exist <- function(...) {
  tryCatch(is_list(cortex_credentials(...)), error = function(e) FALSE)
}

#' @details
#' `cortex_credentials()` picks up the following ambient Snowflake credentials:
#'
#' - A static OAuth token defined via the `SNOWFLAKE_TOKEN` environment
#'   variable.
#' - Key-pair authentication credentials defined via the `SNOWFLAKE_USER` and
#'   `SNOWFLAKE_PRIVATE_KEY` (which can be a PEM-encoded private key or a path
#'   to one) environment variables.
#' - Posit Workbench-managed Snowflake credentials for the corresponding
#'   `account`.
#'
#' @inheritParams chat_cortex
#' @export
#' @rdname chat_cortex
cortex_credentials <- function(account = Sys.getenv("SNOWFLAKE_ACCOUNT")) {
  token <- Sys.getenv("SNOWFLAKE_TOKEN")
  if (nchar(token) != 0) {
    return(
      list(
        Authorization = paste("Bearer", token),
        # See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/authentication#using-oauth
        `X-Snowflake-Authorization-Token-Type` = "OAUTH"
      )
    )
  }

  # Support for Snowflake key-pair authentication.
  # See: https://docs.snowflake.com/en/developer-guide/snowflake-rest-api/authentication#generate-a-jwt-token
  user <- Sys.getenv("SNOWFLAKE_USER")
  private_key <- Sys.getenv("SNOWFLAKE_PRIVATE_KEY")
  if (nchar(user) != 0 && nchar(private_key) != 0) {
    check_installed("jose", "for key-pair authentication")
    key <- openssl::read_key(private_key)
    # We can't use openssl::fingerprint() here because it uses a different
    # algorithm.
    fp <- openssl::base64_encode(
      openssl::sha256(openssl::write_der(key$pubkey))
    )
    sub <- toupper(paste0(account, ".", user))
    iss <- paste0(sub, ".SHA256:", fp)
    # Note: Snowflake employs a malformed issuer claim, so we have to inject it
    # manually after jose's validation phase.
    claim <- httr2::jwt_claim("dummy", sub)
    claim$iss <- iss
    token <- httr2::jwt_encode_sig(claim, key)
    return(
      list(
        Authorization = paste("Bearer", token),
        `X-Snowflake-Authorization-Token-Type` = "KEYPAIR_JWT"
      )
    )
  }

  # Check for Workbench-managed credentials.
  sf_home <- Sys.getenv("SNOWFLAKE_HOME")
  if (grepl("posit-workbench", sf_home, fixed = TRUE)) {
    token <- workbench_snowflake_token(account, sf_home)
    if (!is.null(token)) {
      return(list(
        Authorization = paste("Bearer", token),
        `X-Snowflake-Authorization-Token-Type` = "OAUTH"
      ))
    }
  }

  if (is_testing()) {
    testthat::skip("no Snowflake credentials available")
  }

  cli::cli_abort("No Snowflake credentials are available.")
}

# Reads Posit Workbench-managed Snowflake credentials from a
# $SNOWFLAKE_HOME/connections.toml file, as used by the Snowflake Connector for
# Python implementation. The file will look as follows:
#
# [workbench]
# account = "account-id"
# token = "token"
# authenticator = "oauth"
workbench_snowflake_token <- function(account, sf_home) {
  cfg <- readLines(file.path(sf_home, "connections.toml"))
  # We don't attempt a full parse of the TOML syntax, instead relying on the
  # fact that this file will always contain only one section.
  if (!any(grepl(account, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this account.
    return(NULL)
  }
  line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[line])
  if (nchar(token) == 0) {
    return(NULL)
  }
  # Drop enclosing quotes.
  gsub("\"", "", token)
}
