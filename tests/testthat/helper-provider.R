# Turns ------------------------------------------------------------------

test_turns_system <- function(chat_fun) {
  system_prompt <- "Return very minimal output, AND ONLY USE UPPERCASE."

  chat <- chat_fun(system_prompt = system_prompt)
  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_match(resp, "CHRISTOPHER ROBIN")
  expect_length(chat$turns(), 2)

  chat <- chat_fun(turns = list(Turn("system", system_prompt)))
  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_match(resp, "CHRISTOPHER ROBIN")
  expect_length(chat$turns(), 2)
}

test_turns_existing <- function(chat_fun) {
  chat <- chat_fun(turns = list(
    Turn("system", "Return very minimal output; no punctuation."),
    Turn("user", "List the names of any 8 of Santa's 9 reindeer."),
    Turn("assistant", "Dasher, Dancer, Vixen, Comet, Cupid, Donner, Blitzen, and Rudolph.")
  ))
  expect_length(chat$turns(), 2)

  resp <- chat$chat("Who is the remaining one? Just give the name")
  expect_equal(resp, "Prancer")
  expect_length(chat$turns(), 4)
}


# Tool calls -------------------------------------------------------------

test_tools_simple <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(ToolDef(
    function() "2024-01-01",
    name = "get_date",
    description = "Gets the current date"
  ))

  result <- chat$chat("What's the current date in YMD format?")
  expect_match(result, "2024-01-01")

  result <- chat$chat("What month is it?")
  expect_match(result, "January")
}

test_tools_async <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(ToolDef(
    coro::async(function() "2024-01-01"),
    name = "get_date",
    description = "Gets the current date"
  ))

  result <- sync(chat$chat_async("What's the current date in YMD format?"))
  expect_match(result, "2024-01-01")

  expect_snapshot(chat$chat("Great. Do it again."), error = TRUE)
}

test_tools_parallel <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  favourite_color <- function(person) {
    if (person == "Joe") "sage green" else "red"
  }
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
  expect_identical(result, "Joe: sage green, Hadley: red")
  expect_length(chat$turns(), 4)
}

test_tools_sequential <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
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

  result <- chat$chat("What was the most popular name this year.")
  expect_equal(result, "Susan")
  expect_length(chat$turns(), 6)
}


# Images -----------------------------------------------------------------

test_images_inline <- function(chat_fun) {
  chat <- chat_fun(model = "gpt-4o-mini")
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_file(system.file("httr2.png", package = "elmer"))
  )
  expect_match(response, "hex")
  expect_match(response, "baseball")
}

test_images_remote <- function(chat_fun) {
  chat <- chat_fun(model = "gpt-4o-mini")
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_url("https://httr2.r-lib.org/logo.png")
  )
  expect_match(response, "hex")
  expect_match(response, "baseball")
}
