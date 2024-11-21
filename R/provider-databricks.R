#' Chat with a model hosted on Databricks
#'
#' @description
#' Databricks provides out-of-the-box access to a number of [foundation
#' models](https://docs.databricks.com/en/machine-learning/model-serving/score-foundation-models.html)
#' and can also serve as a gateway for external models hosted by a third party.
#'
#' Databricks models do not support images, but they do support structured
#' outputs. Tool calling support is also very limited at present; too limited
#' for `elmer`'s tool calling features to work properly at all.
#'
#' @family chatbots
#' @param workspace The URL of a Databricks workspace, e.g.
#'   `"https://example.cloud.databricks.com"`. Will use the value of the
#'   environment variable `DATABRICKS_HOST`, if set.
#' @param model The model to use for the chat. The default, `NULL`, will pick a
#'   reasonable default, and tell you about. We strongly recommend explicitly
#'   choosing a model for all but the most casual use. Available foundational
#'   models include:
#'
#'   - `databricks-dbrx-instruct` (the default)
#'   - `databricks-mixtral-8x7b-instruct`
#'   - `databricks-meta-llama-3-1-70b-instruct`
#'   - `databricks-meta-llama-3-1-405b-instruct`
#' @param token An authentication token for the Databricks workspace.
#' @inheritParams chat_openai
#' @inherit chat_openai return
#' @export
chat_databricks <- function(workspace = databricks_workspace(),
                            system_prompt = NULL,
                            turns = NULL,
                            model = NULL,
                            token = databricks_token(workspace),
                            api_args = list(),
                            echo = c("none", "text", "all")) {
  check_string(workspace, allow_empty = FALSE)
  model <- set_default(model, "databricks-dbrx-instruct")
  turns <- normalize_turns(turns, system_prompt)
  echo <- check_echo(echo)
  provider <- ProviderDatabricks(
    base_url = workspace,
    model = model,
    extra_args = api_args,
    api_key = token
  )
  Chat$new(provider = provider, turns = turns, echo = echo)
}

ProviderDatabricks <- new_class(
  "ProviderDatabricks",
  parent = ProviderOpenAI
)

method(chat_request, ProviderDatabricks) <- function(provider,
                                                     stream = TRUE,
                                                     turns = list(),
                                                     tools = list(),
                                                     spec = NULL,
                                                     extra_args = list()) {
  req <- request(provider@base_url)
  # Note: this API endpoint is undocumented and seems to exist primarily for
  # compatibility with the OpenAI Python SDK. The documented endpoint is
  # `/serving-endpoints/<model>/invocations`.
  req <- req_url_path_append(req, "/serving-endpoints/chat/completions")
  # TODO: Support refreshing tokens when needed.
  req <- req_auth_bearer_token(req, provider@api_key)
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) {
    if (resp_content_type(resp) == "application/json") {
      # Databrick's "OpenAI-compatible" API has a slightly incompatible error
      # response format, which we account for here.
      resp_body_json(resp)$message
    }
  })

  messages <- compact(unlist(as_json(provider, turns), recursive = FALSE))
  tools <- as_json(provider, unname(tools))
  extra_args <- utils::modifyList(provider@extra_args, extra_args)

  if (!is.null(spec)) {
    response_format <- list(
      type = "json_schema",
      json_schema = list(
        name = "structured_data",
        schema = as_json(provider, spec),
        strict = TRUE
      )
    )
  } else {
    response_format <- NULL
  }

  data <- compact(list2(
    messages = messages,
    model = provider@model,
    stream = stream,
    tools = tools,
    response_format = response_format,
    !!!extra_args
  ))
  req <- req_body_json(req, data)

  req
}

method(as_json, list(ProviderDatabricks, Turn)) <- function(provider, x) {
  if (x@role == "system") {
    list(list(role = "system", content = x@contents[[1]]@text))
  } else if (x@role == "user") {
    # Each tool result needs to go in its own message with role "tool".
    is_tool <- map_lgl(x@contents, S7_inherits, ContentToolResult)
    if (any(is_tool)) {
      return(lapply(x@contents[is_tool], function(tool) {
        list(role = "tool", content = tool_string(tool), tool_call_id = tool@id)
      }))
    }
    if (length(x@contents) > 1) {
      cli::cli_abort("Databricks models only accept a single text input.")
    }
    content <- as_json(provider, x@contents[[1]])
    list(list(role = "user", content = content))
  } else if (x@role == "assistant") {
    is_tool <- map_lgl(x@contents, S7_inherits, ContentToolRequest)
    if (any(is_tool)) {
      list(list(
        role = "assistant",
        tool_calls = as_json(provider, x@contents[is_tool])
      ))
    } else {
      # We should be able to assume that there is only one content item here.
      content <- as_json(provider, x@contents[[1]])
      list(list(role = "assistant", content = content))
    }
  } else {
    cli::cli_abort("Unknown role {turn@role}", .internal = TRUE)
  }
}

method(as_json, list(ProviderDatabricks, ContentText)) <- function(provider, x) {
  # Databricks only seems to support textual content.
  x@text
}

databricks_workspace <- function() {
  key_get("DATABRICKS_HOST")
}

# Try various ways to get Databricks credentials. This implements a subset of
# the "Databricks client unified authentication" model.
databricks_token <- function(workspace) {
  host <- gsub("https://|/$", "", workspace)

  # An explicit PAT takes precedence over everything else.
  if (nchar(token <- Sys.getenv("DATABRICKS_TOKEN"))) {
    return(token)
  }

  # Next up are explicit OAuth2 M2M credentials.
  client_id <- Sys.getenv("DATABRICKS_CLIENT_ID")
  client_secret <- Sys.getenv("DATABRICKS_CLIENT_SECRET")
  if (nchar(client_id) && nchar(client_secret)) {
    # FIXME: Do the client credentials flow.
    cli::cli_warn("Databricks OAuth M2M credentials are not yet implemented.")
  }

  # Check for Workbench-provided credentials.
  cfg_file <- Sys.getenv("DATABRICKS_CONFIG_FILE")
  if (grepl("posit-workbench", cfg_file, fixed = TRUE)) {
    wb_token <- workbench_databricks_token(host, cfg_file)
    if (!is.null(wb_token)) {
      return(wb_token)
    }
  }

  # When on desktop, try using the Databricks CLI for auth.
  cli_path <- Sys.getenv("DATABRICKS_CLI_PATH", "databricks")
  if (!is_hosted_session() && nchar(Sys.which(cli_path)) != 0) {
    output <- suppressWarnings(
      system2(
        cli_path,
        c("auth", "token", "--host", host),
        stdout = TRUE,
        stderr = TRUE
      )
    )
    output <- paste(output, collapse = "\n")
    # If we don't get an error message, try to extract the token from the JSON-
    # formatted output.
    if (grepl("access_token", output, fixed = TRUE)) {
      token <- gsub(".*access_token\":\\s?\"([^\"]+).*", "\\1", output)
      return(token)
    }
  }

  if (is_testing()) {
    testthat::skip("no Databricks credentials available")
  }

  cli::cli_abort("No Databricks credentials are available.")
}

# Try to determine whether we can redirect the user's browser to a server on
# localhost, which isn't possible if we are running on a hosted platform.
#
# This is based on the strategy pioneered by the {gargle} package and {httr2}.
is_hosted_session <- function() {
  # If RStudio Server or Posit Workbench is running locally (which is possible,
  # though unusual), it's not acting as a hosted environment.
  Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server" &&
    !grepl("localhost", Sys.getenv("RSTUDIO_HTTP_REFERER"), fixed = TRUE)
}

# Reads Posit Workbench-managed Databricks credentials from a
# $DATABRICKS_CONFIG_FILE. The generated file will look as follows:
#
# [workbench]
# host = some-host
# token = some-token
workbench_databricks_token <- function(host, cfg_file) {
  cfg <- readLines(cfg_file)
  # We don't attempt a full parse of the INI syntax supported by Databricks
  # config files, instead relying on the fact that this particular file will
  # always contain only one section.
  if (!any(grepl(host, cfg, fixed = TRUE))) {
    # The configuration doesn't actually apply to this host.
    return(NULL)
  }
  line <- grepl("token = ", cfg, fixed = TRUE)
  token <- gsub("token = ", "", cfg[line])
  if (nchar(token) == 0) {
    return(NULL)
  }
  token
}
