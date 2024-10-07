test_that("can make simple request", {
  chat <- chat_gemini("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")

  resp <- sync(chat$chat_async("What is 1 + 1?"))
  expect_match(resp, "2")
})

test_that("can make simple streaming request", {
  chat <- chat_gemini("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")

  resp <- sync(coro::async_collect(chat$stream_async("1 + 1")))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

test_that("system prompt can be passed explicitly or as a turn", {
  system_prompt <- "Return very minimal output, AND ONLY USE UPPERCASE."

  chat <- chat_gemini(system_prompt = system_prompt)
  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_match(resp, "CHRISTOPHER ROBIN")

  chat <- chat_gemini(turns = list(Turn("system", system_prompt)))
  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_match(resp, "CHRISTOPHER ROBIN")
})

test_that("existing conversation history is used", {
  chat <- chat_gemini(turns = list(
    Turn("system", "Return very minimal output; no punctuation."),
    Turn("user", "List the names of any 8 of Santa's 9 reindeer."),
    Turn("assistant", "Dasher, Dancer, Vixen, Comet, Cupid, Donner, Blitzen, and Rudolph.")
  ))

  resp <- chat$chat("Who is the remaining one? Just give the name")
  expect_match(resp, "Prancer")
})

# Tool calls -------------------------------------------------------------------

test_that("can make a simple tool call", {
  chat <- chat_gemini(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(ToolDef(
    function() "2024-01-01",
    name = "get_date",
    description = "Gets the current date"
  ))

  result <- chat$chat("What's the current date?")
  expect_match(result, "2024-01-01")

  result <- chat$chat("What month is it?")
  expect_match(result, "January")
})

test_that("can make an async tool call", {
  chat <- chat_gemini(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(ToolDef(
    coro::async(function() "2024-01-01"),
    name = "get_date",
    description = "Gets the current date"
  ))

  result <- sync(chat$chat_async("What's the current date?"))
  expect_match(result, "2024-01-01")

  expect_snapshot(chat$chat("Great. Do it again."), error = TRUE)
})

test_that("can call multiple tools in parallel", {
  chat <- chat_gemini(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(ToolDef(
    function(person) if (person == "Joe") "sage green" else "red",
    name = "favourite_color",
    description = "Returns a person's favourite colour",
    arguments = list(person = ToolArg("string", "Name of a person")),
    strict = TRUE
  ))

  result <- chat$chat("
    What are Joe and Hadley's favourite colours?
    Answer like name1: colour1, name2: colour2
  ")
  expect_match(result, "Joe: sage green, Hadley: red")
  expect_length(chat$turns(), 4)
})

test_that("can call multiple tools in sequence", {
  chat <- chat_gemini(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(ToolDef(
    function() 2024,
    name = "get_year",
    description = "Get the current year"
  ))
  chat$register_tool(ToolDef(
    function(year) if (year == 2024) "Susan" else "I don't know",
    name = "popular_name",
    description = "Gets the most popular name for a year",
    arguments = list(year = ToolArg("integer", "Year"))
  ))

  result <- chat$chat("Determine the current year then figure out the most popular name.")
  expect_match(result, "Susan")
  expect_length(chat$turns(), 6)
})

# Images -----------------------------------------------------------------

test_that("can use images (inline and remote)", {
  chat <- chat_gemini()
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_file(system.file("httr2.png", package = "elmer"))
  )
  expect_match(response, "hex")
  expect_match(response, "baseball")
  expect_length(chat$turns(), 2)

  image_remote <- content_image_url("https://httr2.r-lib.org/logo.png")
  expect_snapshot(
    . <- chat$chat("What's in this image?", image_remote),
    error = TRUE
  )
  expect_length(chat$turns(), 2)
})
