test_that("system prompt can be passed explicitly or as message", {
  system_prompt <- "Return very minimal output, AND ONLY USE UPPERCASE."

  chat <- new_chat_openai(system_prompt = system_prompt)

  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_equal(resp, toupper(resp))

  chat <- new_chat_openai(messages = list(
    list(role = "system", content = system_prompt))
  )

  resp <- chat$chat("What is the name of Winnie the Pooh's human friend?")
  expect_equal(resp, toupper(resp))

  expect_error(new_chat_openai(
    system_prompt = system_prompt,
    messages = list(list(role = "system", content = system_prompt))
  ), NA)

  expect_error(new_chat_openai(
    system_prompt = "Answer every question with another question.",
    messages = list(list(role = "system", content = system_prompt))
  ))
})

test_that("existing conversation history is used", {
  chat <- new_chat_openai(messages = list(
    list(role = "system", content = "Return very minimal output."),
    list(role = "user", content = "List the names of any 8 of Santa's 9 reindeer."),
    list(role = "assistant", content = "Dasher, Dancer, Vixen, Comet, Cupid, Donner, Blitzen, and Rudolph.")
  ))

  resp <- chat$chat("Who is the remaining one?")
  expect_match(resp, "Prancer\\.?")
})
