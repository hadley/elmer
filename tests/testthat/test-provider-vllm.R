# Getting started --------------------------------------------------------

test_that("can make simple request", {
  chat <- chat_vllm_test("Be as terse as possible; no punctuation")
  resp <- chat$chat("What is 1 + 1?", echo = FALSE)
  expect_match(resp, "2")
  expect_equal(chat$last_turn()@tokens, c(64, 2))
})

test_that("can make simple streaming request", {
  chat <- chat_vllm_test("Be as terse as possible; no punctuation")
  resp <- coro::collect(chat$stream("What is 1 + 1?"))
  expect_match(paste0(unlist(resp), collapse = ""), "2")
})

# Common provider interface -----------------------------------------------

test_that("respects turns interface", {
  chat_fun <- chat_vllm_test

  test_turns_system(chat_fun)
  test_turns_existing(chat_fun)
})
