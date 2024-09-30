test_that("invalid inputs give useful errors", {
  chat <- new_chat_openai()

  expect_snapshot(error = TRUE, {
    chat$chat(question = "Are unicorns real?")
    chat$chat(TRUE)
    chat$chat(list())
    chat$chat(list(type = "unicorn"))
    chat$chat(list(type = "text"))
    chat$chat(list(type = "text", text = NULL))
  })
})

test_that("inputs are validated", {

  expect_identical(
    normalize_chat_input("Simple string"),
    list(role = "user", content = "Simple string")
  )

  expect_identical(
    normalize_chat_input(!!!letters[1:3]),
    list(
      role = "user",
      content = lapply(
        letters[1:3],
        function(x) list(type = "text", text = x)
      )
    )
  )

  expect_identical(
    normalize_chat_input(letters[1:3]),
    list(
      role = "user",
      content = "a\nb\nc"
    )
  )

  expect_identical(
    normalize_chat_input(letters[1:3], letters[4:6]),
    list(
      role = "user",
      content = list(
        list(type = "text", text = "a\nb\nc"),
        list(type = "text", text = "d\ne\nf")
      )
    )
  )

  expect_identical(
    normalize_chat_input("one", "two", list(type = "text", text = "three")),
    list(
      role = "user",
      content = lapply(
        c("one", "two", "three"),
        function(x) list(type = "text", text = x)
      )
    )
  )
})
