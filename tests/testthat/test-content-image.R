test_that("can create image from url", {
  obj <- content_image_url("https://www.r-project.org/Rlogo.png")
  expect_s3_class(obj, "elmer::content_image_remote")
})

test_that("can create image from path", {
  skip_if_not_installed("magick")

  path <- system.file("httr2.png", package = "elmer")
  obj <- content_image_file(path)
  expect_s3_class(obj, "elmer::content_image_inline")
})

test_that("can create image from plot", {
  withr::local_pdf(NULL)
  dev.control("enable")
  plot(1:10)

  obj <- content_image_plot()
  expect_s3_class(obj, "elmer::content_image_inline")
  expect_equal(obj@type, "image/png")
})

test_that("image resizing", {
  skip_if_not_installed("magick")

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
