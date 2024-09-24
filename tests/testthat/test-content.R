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

test_that("image resizing", {
  img_file <- system.file("httr2.png", package = "elmer")

  expect_snapshot(error = TRUE, {
    content_image_file("DOESNTEXIST")
    content_image_file(test_path("test-content.R"))
    content_image_file(img_file, resize = TRUE)
    content_image_file(img_file, resize = "blah")
  })

  expect_no_error(content_image_file(img_file))
  expect_no_error(content_image_file(img_file, resize = "low"))
  expect_no_error(content_image_file(img_file, resize = "high"))
  expect_no_error(content_image_file(img_file, resize = "none"))
  expect_no_error(content_image_file(img_file, resize = "100x100"))
  expect_no_error(content_image_file(img_file, resize = "100x100>!"))
})

test_that("useful errors if no display", {
  # file based devices have display list inhibited
  withr::local_pdf(NULL)
  expect_snapshot(content_image_plot(), error = TRUE)
})
