# normalize_messages throws useful errors

    Code
      normalize_messages(1)
    Condition
      Error:
      ! `messages` must be an unnamed list or `NULL`, not the number 1.
    Code
      normalize_messages(list(1))
    Condition
      Error in `normalize_messages()`:
      ! Every element of `messages` must be a `chat_message`.
    Code
      normalize_messages(list(sys_msg, user_msg), 1)
    Condition
      Error:
      ! `system_prompt` must be a single string or `NULL`, not the number 1.
    Code
      normalize_messages(list(sys_msg, user_msg), "foo2")
    Condition
      Error:
      ! `system_prompt` and `messages[[1]]` can't contain conflicting system prompts.

