test_that("image query", {
  img_file <- system.file("httr2.png", package = "elmer")

  chat <- new_chat_openai(model = "gpt-4o-mini")
  response <- chat$chat(
    "What's in this image? (Be sure to mention the outside shape)",
    content_image_file(img_file, resize = "150x150")
  )
  expect_match(response, "hex")
  expect_match(response, "baseball")
})

test_that("inputs are validated", {
  chat <- new_chat_openai()

  expect_error(chat$chat(1))
  expect_error(chat$chat(TRUE))
  expect_error(chat$chat(list()))
  expect_error(chat$chat(question = "Are unicorns real?"))

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

test_that("image resizing", {
  img_file <- system.file("httr2.png", package = "elmer")

  expect_error(content_image_file(img_file, resize = TRUE))
  expect_error(content_image_file(img_file, resize = "blah"))

  expect_error(content_image_file(img_file), NA)
  expect_error(content_image_file(img_file, resize = "low"), NA)
  expect_error(content_image_file(img_file, resize = "high"), NA)
  expect_error(content_image_file(img_file, resize = FALSE), NA)
  expect_error(content_image_file(img_file, resize = "100x100"), NA)
  expect_error(content_image_file(img_file, resize = "100x100>!"), NA)
})
