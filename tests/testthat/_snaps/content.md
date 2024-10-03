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
      Error in `FUN()`:
      ! `...` must be made up strings or <content> objects, not `TRUE`.

