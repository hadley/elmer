openai_chat <- function(messages,
                        tools = list(),
                        base_url = "https://api.openai.com/v1",
                        model = "gpt-4o-mini",
                        stream = TRUE,
                        seed = NULL,
                        api_key = openai_key()) {
  req <- openai_chat_req(
    messages = messages,
    tools = tools,
    base_url = base_url,
    model = model,
    stream = stream,
    seed = seed,
    api_key = api_key
  )

  if (stream) {
    openai_chat_stream(req)
  } else {
    resp <- req_perform(req)
    resp_body_json(resp)
  }
}

openai_chat_stream <- NULL
rlang::on_load(openai_chat_stream <- coro::generator(function(req) {
  resp <- req_perform_connection(req)
  on.exit(close(resp))
  reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      abort("Connection failed")
    }
    if (event$data == "[DONE]") {
      break
    }
    json <- jsonlite::parse_json(event$data)
    yield(json)
  }

  # Work around https://github.com/r-lib/coro/issues/51
  if (FALSE) {
    yield(NULL)
  }
}))

openai_chat_async <- function(messages,
                              tools = list(),
                              base_url = "https://api.openai.com/v1",
                              model = "gpt-4o-mini",
                              stream = TRUE,
                              seed = NULL,
                              api_key = openai_key()) {
  req <- openai_chat_req(
    messages = messages,
    tools = tools,
    base_url = base_url,
    model = model,
    stream = stream,
    seed = seed,
    api_key = api_key
  )

  if (stream) {
    openai_chat_stream_async(req)
  } else {
    resp <- req_perform_promise(req)
    promises::then(resp, resp_body_json)
  }
}

openai_chat_stream_async <- NULL
rlang::on_load(openai_chat_stream_async <- coro::async_generator(function(req, polling_interval_secs = 0.1) {
  resp <- req_perform_connection(req, blocking = FALSE)
  on.exit(close(resp))
  # TODO: Investigate if this works with async generators
  # reg.finalizer(environment(), function(e) { close(resp) }, onexit = FALSE)

  while (TRUE) {
    event <- resp_stream_sse(resp)
    if (is.null(event)) {
      # TODO: Detect if connection is closed and stop polling
      await(coro::async_sleep(polling_interval_secs))
    } else if (event$data == "[DONE]") {
      break
    } else {
      json <- jsonlite::parse_json(event$data)
      yield(json)
    }
  }

  # Work around https://github.com/r-lib/coro/issues/51
  if (FALSE) {
    yield(NULL)
  }
}))

openai_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (identical(key, "")) {
    cli::cli_abort("Can't find env var {.code OPENAI_API_KEY}.")
  }
  key
}

# https://platform.openai.com/docs/api-reference/chat/create
openai_chat_req <- function(messages,
                            base_url = "https://api.openai.com/v1",
                            model = "gpt-4o-mini",
                            frequency_penalty = NULL,
                            logit_bias = NULL,
                            logprobs = NULL,
                            top_logprobs = NULL,
                            max_completion_tokens = NULL,
                            n = NULL,
                            presence_penalty = NULL,
                            response_format = NULL,
                            seed = NULL,
                            service_tier = NULL,
                            stop = NULL,
                            stream = TRUE,
                            stream_options = NULL,
                            temperature = NULL,
                            top_p = NULL,
                            tools = list(),
                            tool_choice = NULL,
                            parallel_tool_calls = NULL,
                            user = NULL,
                            api_key = openai_key()) {

  check_string(model)
  check_number_decimal(frequency_penalty, min = -2, max = 2, allow_null = TRUE)
  # logit_bias:  named list of integers that lets you boost/suppress individual tokens
  check_bool(logprobs, allow_null = TRUE)
  check_number_whole(top_logprobs, min = 0, max = 20, allow_null = TRUE)
  check_number_whole(max_completion_tokens, min = 1, allow_null = TRUE)
  check_number_whole(n, min = 1, allow_null = TRUE)
  check_number_decimal(presence_penalty, min = -2, max = 2, allow_null = TRUE)
  # response_format: object specifiying output format
  check_number_whole(seed, allow_null = TRUE)
  check_string(service_tier, allow_null = TRUE)
  check_character(stop, allow_null = TRUE)
  check_bool(stream)
  # stream_options: object specifying stream options
  check_number_decimal(temperature, min = 0, max = 2, allow_null = TRUE)
  check_number_decimal(top_p, min = 0, max = 1, allow_null = TRUE)
  # tools: list of tools to use
  # tool_choice: object specifying tool choice
  check_bool(parallel_tool_calls, allow_null = TRUE)
  check_string(user, allow_null = TRUE)

  data <- compact(list(
    messages = messages,
    model = model,
    frequency_penalty = frequency_penalty,
    logit_bias = logit_bias,
    logprobs = logprobs,
    top_logprobs = top_logprobs,
    max_completion_tokens = max_completion_tokens,
    n = n,
    presence_penalty = presence_penalty,
    response_format = response_format,
    seed = seed,
    service_tier = service_tier,
    stop = stop,
    stream = stream,
    stream_options = stream_options,
    temperature = temperature,
    top_p = top_p,
    tools = tools,
    tool_choice = tool_choice,
    parallel_tool_calls = parallel_tool_calls,
    user = user
  ))

  req <- openai_request(base_url = base_url, key = api_key)
  req <- req_url_path_append(req, "/chat/completions")
  req <- req_body_json(req, data)
  req
}

openai_request <- function(base_url = "https://api.openai.com/v1",
                           key = openai_key()) {
  req <- request(base_url)
  req <- req_auth_bearer_token(req, Sys.getenv("OPENAI_API_KEY"))
  req <- req_retry(req, max_tries = 2)
  req <- req_error(req, body = function(resp) {
     resp_body_json(resp)$error$message
  })
  # req <- req_verbose(req, body_req = TRUE, body_resp = TRUE)
  req
}

openai_key_exists <- function() {
  !identical(Sys.getenv("OPENAI_API_KEY"), "")
}

openai_key <- function() {
  if (openai_key_exists()) {
    Sys.getenv("OPENAI_API_KEY")
  } else {
    if (is_testing()) {
      testthat::skip("OPENAI_API_KEY env var is not configured")
    } else {
      cli::cli_abort("Can't find env var {.code OPENAI_API_KEY}.")
    }
  }
}
