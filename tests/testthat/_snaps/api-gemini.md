# can make an async tool call

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error in `FUN()`:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

# can use images (inline and remote)

    Code
      . <- chat$chat("What's in this image?", image_remote)
    Condition
      Error:
      ! Gemini doesn't support remote images

