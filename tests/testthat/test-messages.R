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
