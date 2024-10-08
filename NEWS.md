# elmer (development version)

* The `echo` can now be one of three values: "none", "text", or "all". If "all", you'll now see both user and assistant turns, and all content types will be printed, not just text.

* `chat$register_tool()` now takes an object created by `ToolDef()`. This makes it a little easier to reuse tool definitions (#32).

* `new_chat_openai()` is now `chat_openai()`.

* Claude and Gemini are now supported via `chat_claude()` and `chat_gemini()`.
