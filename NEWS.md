# elmer (development version)

* The `echo` can now be one of three values: "none", "text", or "all". If "all", you'll now see both user and assistant turns, and all content types will be printed, not just text.

* `chat$register_tool()` now takes an object created by `ToolDef()`. This makes it a little easier to reuse tool definitions (#32).
