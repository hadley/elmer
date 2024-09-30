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
      Error in `chat$chat()`:
      ! input content must be a string or list, not `TRUE`.
    Code
      chat$chat(list(type = "unicorn"))
    Condition
      Error in `chat$chat()`:
      ! Unknown type "unicorn" in list item.
    Code
      chat$chat(list(type = "text"))
    Condition
      Error in `chat$chat()`:
      ! List item must have a 'text' field.
    Code
      chat$chat(list(type = "text", text = NULL))
    Condition
      Error:
      ! <elmer::content_text> object properties are invalid:
      - @text must be <character>, not <NULL>

