# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_openai("Be as terse as possible; no punctuation", model = "gpt-4o-mini")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(27, 1))
})

test_that("can make simple streaming request", {
  chat <- chat_openai("Be as terse as possible; no punctuation", model = "gpt-4o-mini")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("defaults are reported", {
  expect_snapshot(. <- chat_openai())
})

test_that("respects turns interface", {
  chat_fun <- function(...) chat_openai(..., model = "gpt-4o-mini")

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})

test_that("all tool variations work", {
  chat_fun <- function(...) chat_openai(..., model = "gpt-4o-mini")

  test_tools_simple(chat_fun)
  test_tools_async(chat_fun)
  test_tools_parallel(chat_fun)
  test_tools_sequential(chat_fun, total_calls = 6)
})

test_that("can extract data", {
  chat_fun <- function(...) chat_openai(..., model = "gpt-4o-mini")

  test_data_extraction(chat_fun)
})

test_that("can use images", {
  chat_fun <- function(...) chat_openai(..., model = "gpt-4o-mini")

  test_images_inline(chat_fun)
  test_images_remote(chat_fun)
})

# Custom tests -----------------------------------------------------------------

test_that("can retrieve logprobs (#115)", {
  chat <- chat_openai(api_args = list(logprobs = TRUE))
  pieces <- coro::collect(chat$stream("Hi"))

  logprops <- chat$last_turn()@json$choices[[1]]$logprobs$content
  expect_equal(
    length(logprops),
    length(pieces) - 2 # leading "" + trailing \n
  )
})

# Custom -----------------------------------------------------------------

test_that("as_json specialised for OpenAI", {
  stub <- ProviderOpenAI(base_url = "", api_key = "", model = "")

  expect_snapshot(
    as_json(stub, type_object(.additional_properties = TRUE)),
    error = TRUE
  )

  obj <- type_object(x = type_number(required = FALSE))
  expect_equal(
    as_json(stub, obj),
    list(
      type = "object",
      description = "",
      properties = list(x = list(type = c("number", "null"), description = "")),
      required = list("x"),
      additionalProperties = FALSE
    )
  )
})
