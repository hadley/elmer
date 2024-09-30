test_that("invalid inputs give useful errors", {
  chat <- new_chat_openai()

  expect_snapshot(error = TRUE, {
    chat$chat(question = "Are unicorns real?")
    chat$chat(TRUE)
    chat$chat(list(type = "unicorn"))
    chat$chat(list(type = "text"))
    chat$chat(list(type = "text", text = NULL))
  })
})

test_that("inputs are validated", {
  openai <- new_openai_provider(model = "gpt-4o")

  expect_equal(
    normalize_content(openai, "Simple string"),
    list(content_text("Simple string"))
  )

  expect_equal(
    normalize_content(openai, !!!letters[1:3]),
    list(
      content_text("a"),
      content_text("b"),
      content_text("c")
    )
  )

  expect_equal(
    normalize_content(openai, letters[1:3]),
    list(content_text("a\nb\nc"))
  )

  expect_equal(
    normalize_content(openai, letters[1:3], letters[4:6]),
    list(
      content_text("a\nb\nc"),
      content_text("d\ne\nf")
    )
  )

  expect_equal(
    normalize_content(
      openai,
      "one",
      list(type = "text", text = "two"),
      content_text("three")
    ),
    list(
      content_text("one"),
      content_text("two"),
      content_text("three")
    )
  )
})
