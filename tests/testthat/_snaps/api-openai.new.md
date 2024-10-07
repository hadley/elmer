# default model is reported

    Code
      . <- chat_openai()$chat("Hi")
    Message
      Using model = "gpt-4o-mini".
    Condition
      Warning in `turn()`:
      partial argument match of 'content' to 'contents'

# can make an async tool call

    Code
      chat$chat("Great. Do it again.")
    Condition
      Warning in `turn()`:
      partial argument match of 'content' to 'contents'
      Error in `FUN()`:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

