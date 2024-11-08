# Request an OAuth access token for the given resource from Posit Connect. The
# OAuth token will belong to the user owning the given Shiny session.
connect_viewer_token <- function(session, resource) {
  if (!running_on_connect()) {
    cli::cli_inform(c(
      "!" = "Ignoring the {.arg sesssion} parameter.",
      "i" = "Viewer-based credentials are only available when running on Connect."
    ))
    ## ), .frequency = "once", .frequency_id = "session param")
    return(NULL)
  }

  # Older versions or certain configurations of Connect might not supply a user
  # session token.
  server_url <- Sys.getenv("CONNECT_SERVER")
  token <- session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
  if (is.null(token) || nchar(server_url) == 0) {
    cli::cli_abort(
      "Viewer-based credentials are not supported by this version of Connect."
    )
  }

  # See: https://docs.posit.co/connect/api/#post-/v1/oauth/integrations/credentials
  req <- httr2::request(server_url)
  req <- httr2::req_url_path_append(
    req, "__api__/v1/oauth/integrations/credentials"
  )
  req <- httr2::req_headers(req,
    Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY")),
    .redact = "Authorization"
  )
  req <- httr2::req_body_form(
    req,
    grant_type = "urn:ietf:params:oauth:grant-type:token-exchange",
    subject_token_type = "urn:posit:connect:user-session-token",
    subject_token = token,
    resource = resource
  )

  # TODO: Do we need more precise error handling?
  req <- httr2::req_error(
    req, body = function(resp) httr2::resp_body_json(resp)$error
  )

  resp <- httr2::resp_body_json(httr2::req_perform(req))
  resp$access_token
}

running_on_connect <- function() {
  Sys.getenv("RSTUDIO_PRODUCT") == "CONNECT"
}

check_shiny_session <- function(x,
                                ...,
                                allow_null = FALSE,
                                arg = caller_arg(x),
                                call = caller_env()) {
  if (!missing(x)) {
    if (inherits(x, "ShinySession")) {
      return(invisible(NULL))
    }
    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }
  stop_input_type(
    x,
    "a Shiny session object",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}
