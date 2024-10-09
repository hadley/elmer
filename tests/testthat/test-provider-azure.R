test_that("can make simple request", {
  chat <- chat_azure(
    system_prompt = "Be as terse as possible; no punctuation",
    endpoint = "https://ai-hwickhamai260967855527.openai.azure.com",
    deployment_id = "gpt-4o-mini"
  )
  resp <- chat$chat("What is 1 + 1?")
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(27, 1))

  resp <- sync(chat$chat_async("What is 1 + 1?"))
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(44, 1))
})
