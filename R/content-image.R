
#' Encode image content for chat input
#'
#' These functions are used to prepare image URLs and files for input to the
#' chatbot. The `content_image_url()` function is used to provide a URL to an
#' image, while `content_image_file()` is used to provide the image data itself.
#'
#' @param url The URL of the image to include in the chat input. Can be a
#'   `data:` URL or a regular URL. Valid image types are PNG, JPEG, WebP, and
#'   non-animated GIF.
#' @param detail The [detail
#'   setting](https://platform.openai.com/docs/guides/vision/low-or-high-fidelity-image-understanding)
#'   for this image. Can be `"auto"`, `"low"`, or `"high"`.
#' @returns An input object suitable for including in the `...` parameter of
#'   the `chat()`, `stream()`, `chat_async()`, or `stream_async()` methods.
#'
#' @export
#' @examplesIf elmer:::openai_key_exists()
#' chat <- new_chat_openai(echo = TRUE)
#' chat$chat(
#'   "What do you see in these images?",
#'   content_image_url("https://www.r-project.org/Rlogo.png"),
#'   content_image_file(system.file("httr2.png", package = "elmer"))
#' )
#'
#' \dontshow{dev.control('enable')}
#' plot(waiting ~ eruptions, data = faithful)
#' chat <- new_chat_openai(echo = TRUE)
#' chat$chat(
#'   "Describe this plot in one paragraph, as suitable for inclusion in
#'    alt-text. You should briefly describe the plot type, the axes, and
#'    2-5 major visual patterns.",
#'    content_image_plot()
#' )
content_image_url <- function(url, detail = c("auto", "low", "high")) {
  detail <- arg_match(detail)
  content_image_remote(url = url, detail = detail)
}

#' @rdname content_image_url
#' @param path The path to the image file to include in the chat input. Valid
#'   file extensions are `.png`, `.jpeg`, `.jpg`, `.webp`, and (non-animated)
#'   `.gif`.
#' @param content_type The content type of the image (e.g. `image/png`). If
#'   `"auto"`, the content type is inferred from the file extension.
#' @param resize If `"low"`, resize images to fit within 512x512. If `"high"`,
#'   resize to fit within 2000x768 or 768x2000. (See the [OpenAI
#'   docs](https://platform.openai.com/docs/guides/vision/low-or-high-fidelity-image-understanding)
#'   for more on why these specific sizes are used.) If `"none"`, do not resize.
#'
#'   You can also pass a custom string to resize the image to a specific size,
#'   e.g. `"200x200"` to resize to 200x200 pixels while preserving aspect ratio.
#'   Append `>` to resize only if the image is larger than the specified size,
#'   and `!` to ignore aspect ratio (e.g. `"300x200>!"`).
#'
#'   All values other than `none` require the `magick` package.
#' @export
content_image_file <- function(path, content_type = "auto", resize = "low") {
  rlang::check_installed("base64enc", "to encode images")

  # TODO: Allow vector input?
  check_string(path, allow_empty = FALSE)
  check_string(content_type, allow_empty = FALSE)
  check_string(resize, allow_empty = FALSE)

  if (!file.exists(path) || dir.exists(path)) {
    cli::cli_abort("{path} must be an existing file.")
  }

  if (content_type == "auto") {
    # OpenAI supports .png, .jpeg, .jpg, .webp, .gif
    # https://platform.openai.com/docs/guides/vision/what-type-of-files-can-i-upload
    ext <- tolower(tools::file_ext(path))
    content_type <- switch(
      ext,
      png = "image/png",
      jpeg = "image/jpeg",
      jpg = "image/jpeg",
      webp = "image/webp",
      gif = "image/gif",
      cli::cli_abort("Unsupported image file extension: {ext}.")
    )
  }

  # Implement resizing logic
  if (resize == "none") {
    base64 <- base64enc::base64encode(path)
  } else {
    rlang::check_installed("magick", "to resize images")

    img <- magick::image_read(path)

    if (resize == "low") {
      img <- magick::image_resize(img, "512x512>")
    } else if (resize == "high") {
      # Get current image dimensions
      dims <- magick::image_info(img)
      width <- dims$width
      height <- dims$height

      if (width > height) {
        img <- magick::image_resize(img, "2000x768>")
      } else {
        img <- magick::image_resize(img, "768x2000>")
      }
    } else {
      img <- magick::image_resize(img, resize)
    }
    buf <- magick::image_write(img, format = magick::image_info(img)$format)
    base64 <- base64enc::base64encode(buf)
  }

  content_image_inline(content_type, base64)
}

#' @rdname content_image_url
#' @export
#' @param width,height Width and height in pixels.
content_image_plot <- function(width = 768, height = 768) {
  plot <- grDevices::recordPlot()

  if (is.null(plot[[1]])) {
    cli::cli_abort(c(
      "Can't record plot because display list is inhibited.",
      i = "Turn it on with {.code dev.control('enable')}."
    ))
  }

  old <- grDevices::dev.cur()

  path <- tempfile("elmer-plot-", fileext = ".png")
  defer(unlink(path))

  grDevices::png(path, width = width, height = height)
  grDevices::replayPlot(plot)
  grDevices::dev.off()

  grDevices::dev.set(old)

  content_image_inline(path, "image/png", resize = "high")
}
