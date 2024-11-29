test_that("invalid inputs give useful errors", {
  chat <- chat_openai()

  expect_snapshot(error = TRUE, {
    chat$chat(question = "Are unicorns real?")
    chat$chat(TRUE)
  })
})

test_that("can create content from a vector", {
  expect_equal(
    as_content(c("a", "b")),
    ContentText("a\n\nb")
  )
})

test_that("turn contents can be converted to text, markdown and HTML", {
  turn <- Turn(
    "user",
    contents = list(
      ContentText("User input."),
      ContentImageInline("image/png", "abcd123"),
      ContentImageRemote("https://example.com/image.jpg", detail = ""),
      ContentJson(list(a = 1:2, b = "apple")),
      ContentSql("SELECT * FROM mtcars"),
      ContentSuggestions(
        c(
          "What is the total quantity sold for each product last quarter?",
          "What is the average discount percentage for orders from the United States?",
          "What is the average price of products in the 'electronics' category?"
        )
      )
    )
  )

  expect_snapshot(cat(contents_text(turn)))
  expect_snapshot(cat(contents_markdown(turn)))

  turns <- list(
    turn,
    Turn("assistant", list(ContentText("Here's your answer.")))
  )
  chat <- Chat$new(Provider("https://example.com/api"), turns = turns)
  expect_snapshot(cat(contents_markdown(chat)))

  skip_if_not_installed("commonmark")
  expect_snapshot(cat(contents_html(turn)))
})
