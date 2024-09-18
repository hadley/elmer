# invalid inputs give useful errors

    Code
      chat$chat(question = "Are unicorns real?")
    Condition
      Error in `chat$chat()`:
      ! Arguments in `...` must be passed by position, not name.
      x Problematic argument:
      * question = "Are unicorns real?"
    Code
      chat$chat(TRUE)
    Condition
      Error in `chat$chat()`:
      ! input content must be a string or list, not `TRUE`.
    Code
      chat$chat(list())
    Condition
      Error in `chat$chat()`:
      ! List item must have a 'type' field.
    Code
      chat$chat(list(type = "unicorn"))
    Condition
      Error in `chat$chat()`:
      ! `type` must be one of "text" or "image_url", not "unicorn".
    Code
      chat$chat(list(type = "text"))
    Condition
      Error in `chat$chat()`:
      ! List item of type 'text' must have a 'text' field.
    Code
      chat$chat(list(type = "text", text = NULL))
    Condition
      Error in `chat$chat()`:
      ! 'text' field cannot be NULL.

# image resizing

    Code
      content_image_file("DOESNTEXIST")
    Condition
      Error in `content_image_file()`:
      ! DOESNTEXIST must be an existing file.
    Code
      content_image_file(test_path("test-content.R"))
    Condition
      Error in `content_image_file()`:
      ! Unsupported image file extension: r.
    Code
      content_image_file(img_file, resize = TRUE)
    Condition
      Error in `content_image_file()`:
      ! `resize` must be a single string, not `TRUE`.
    Code
      content_image_file(img_file, resize = "blah")
    Condition
      Error:
      ! Invalid geometry string: blah

