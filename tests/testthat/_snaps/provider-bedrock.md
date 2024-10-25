# defaults are reported

    Code
      . <- chat_bedrock()
    Message
      Using model = "anthropic.claude-3-5-sonnet-20240620-v1:0".

# all tool variations work

    Code
      chat$chat("Great. Do it again.")
    Condition
      Error in `FUN()`:
      ! Can't use async tools with `$chat()` or `$stream()`.
      i Async tools are supported, but you must use `$chat_async()` or `$stream_async()`.

