# errors with invalid data urls

    Code
      content_image_url("data:base64,abcd")
    Condition
      Error in `content_image_url()`:
      ! `url` is not a valid data url.

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

# useful errors if no display

    Code
      content_image_plot()
    Condition
      Error in `content_image_plot()`:
      ! Can't record plot because display list is inhibited.
      i Turn it on with `dev.control('enable')`.

