retry_test <- function(code, retries = 1) {
  code <- enquo(code)

  i <- 1
  while (i <= retries) {
    tryCatch(
      {
        return(eval(get_expr(code), get_env(code)))
        break
      },
      expectation_failure = function(cnd) NULL
    )
    i <- i + 1
  }

  eval(get_expr(code), get_env(code))
}

# Turns ------------------------------------------------------------------

test_turns_system <- function(chat_fun) {
  system_prompt <- "Return very minimal output, AND ONLY USE UPPERCASE."

  chat <- chat_fun(system_prompt = system_prompt)
  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_match(resp, "CHRISTOPHER ROBIN")
  expect_length(chat$get_turns(), 2)

  chat <- chat_fun(turns = list(Turn("system", system_prompt)))
  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_match(resp, "CHRISTOPHER ROBIN")
  expect_length(chat$get_turns(), 2)
}

test_turns_existing <- function(chat_fun) {
  chat <- chat_fun(turns = list(
    Turn("system", "Return very minimal output; no punctuation."),
    Turn("user", "List the names of any 8 of Santa's 9 reindeer."),
    Turn("assistant", "Dasher, Dancer, Vixen, Comet, Cupid, Donner, Blitzen, and Rudolph.")
  ))
  expect_length(chat$get_turns(), 2)

  resp <- chat$chat("Who is the remaining one? Just give the name")
  expect_match(resp, "Prancer")
  expect_length(chat$get_turns(), 4)
}

# Tool calls -------------------------------------------------------------

test_tools_simple <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(tool(function() "2024-01-01", "Gets the current date"))

  result <- chat$chat("What's the current date in YMD format?")
  expect_match(result, "2024-01-01")

  result <- chat$chat("What month is it? Provide the full name")
  expect_match(result, "January")
}

test_tools_async <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  chat$register_tool(tool(coro::async(function() "2024-01-01"), "Gets the current date"))

  result <- sync(chat$chat_async("What's the current date in YMD format?"))
  expect_match(result, "2024-01-01")

  expect_snapshot(chat$chat("Great. Do it again."), error = TRUE)
}

test_tools_parallel <- function(chat_fun) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")
  favourite_color <- function(person) {
    if (person == "Joe") "sage green" else "red"
  }
  chat$register_tool(tool(
    favourite_color,
    "Returns a person's favourite colour",
    person = type_string("Name of a person")
  ))

  result <- chat$chat("
    What are Joe and Hadley's favourite colours?
    Answer like name1: colour1, name2: colour2
  ")
  expect_match(result, "Joe: sage green")
  expect_match(result, "Hadley: red")
  expect_length(chat$get_turns(), 4)
}

test_tools_sequential <- function(chat_fun, total_calls) {
  chat <- chat_fun(system_prompt = "Be very terse, not even punctuation.")

  current_year <- function() 2024
  popular_name <- function(year) if (year == 2024) "Susan" else "I don't know"
  chat$register_tool(tool(current_year, "Get the current year"))
  chat$register_tool(tool(
    popular_name,
    "Gets the most popular name for a year",
    year = type_integer("Year")
  ))

  result <- chat$chat("What was the most popular name this year.")
  expect_match(result, "Susan")
  expect_length(chat$get_turns(), total_calls)
}

# Data extraction --------------------------------------------------------

test_data_extraction <- function(chat_fun) {
  article_summary <- type_object(
    "Summary of the article.",
    title = type_string("Content title"),
    author = type_string("Name of the author")
  )

  prompt <- "
    # Apples are tasty
    By Hadley Wickham

    Apples are delicious and tasty and I like to eat them.
    Except for red delicious, that is. They are NOT delicious.
  "

  chat <- chat_fun()
  data <- chat$extract_data(prompt, spec = article_summary)
  expect_mapequal(data, list(title = "Apples are tasty", author = "Hadley Wickham"))

  # Check that we can do it again
  data <- chat$extract_data(prompt, spec = article_summary)
  expect_mapequal(data, list(title = "Apples are tasty", author = "Hadley Wickham"))
}

# Images -----------------------------------------------------------------

test_images_inline <- function(chat_fun) {
  chat <- chat_fun()
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_file(system.file("httr2.png", package = "elmer"))
  )
  expect_match(response, "hex")
  expect_match(response, "baseball")
}

test_images_remote <- function(chat_fun) {
  chat <- chat_fun()
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_url("https://httr2.r-lib.org/logo.png")
  )
  expect_match(response, "hex")
  expect_match(response, "baseball")
}

test_images_remote_error <- function(chat_fun) {
  chat <- chat_fun()

  image_remote <- content_image_url("https://httr2.r-lib.org/logo.png")
  expect_snapshot(
    . <- chat$chat("What's in this image?", image_remote),
    error = TRUE
  )
  expect_length(chat$get_turns(), 0)
}
