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

# Define allowed types - add new types here in the future
allowed_input_types <- c("text", "image_url")

normalize_chat_input <- function(..., error_call = caller_env()) {
  check_dots_unnamed(call = error_call)
  input <- rlang::list2(...)

  if (length(input) == 1 && is.character(input[[1]])) {
    # The common case of just a string, can be left as a string
    content <- paste(input[[1]], collapse = "\n")
  } else {
    # Otherwise, process all elements
    content <- lapply(input, process_single_input, error_call = error_call)
  }

  list(role = "user", content = content)
}

process_single_input <- function(item, error_call = caller_env()) {
  if (is.character(item)) {
    # If item is a string, convert it to text format
    list(type = "text", text = paste(item, collapse = "\n"))
  } else if (S7_inherits(item, content_image)) {
    item
  } else if (is.list(item)) {
    if (!"type" %in% names(item)) {
      cli::cli_abort("List item must have a 'type' field.", call = error_call)
    }

    type <- item[["type"]]
    type <- arg_match(type, allowed_input_types, error_call = error_call)

    if (!type %in% names(item)) {
      cli::cli_abort("List item of type '{type}' must have a '{type}' field.", call = error_call)
    }

    if (is.null(item[[type]])) {
      cli::cli_abort("'{type}' field cannot be NULL.", call = error_call)
    }

    if (type == "text") {
      item[["text"]] <- paste(item[[type]], collapse = "\n")
    }

    item
  } else {
    stop_input_type(
      item,
      "a string or list",
      arg = I("input content"),
      call = error_call
    )
  }
}

prop_string <- function(allow_null = FALSE, allow_na = FALSE) {
  force(allow_null)
  force(allow_na)

  new_property(
    class = if (allow_null) NULL | class_character else class_character,
    validator = function(value) {
      if (length(value) != 1) {
        "Must be a single string"
      } else if (!allow_na && is.na(value)) {
        "Must not be missing"
      }
    }
  )
}


content_image <- new_class("content_image")

content_image_remote <- new_class(
  "content_image_remote",
  parent = content_image,
  properties = list(
    url = prop_string(),
    detail = prop_string()
  ),
  package = "elmer"
)
content_image_inline <- new_class(
  "content_image_inline",
  parent = content_image,
  properties = list(
    type = prop_string(),
    data = prop_string(allow_null = TRUE)
  ),
  package = "elmer"
)
