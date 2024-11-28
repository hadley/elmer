# normalize_turns throws useful errors

    Code
      normalize_turns(1)
    Condition
      Error:
      ! `turns` must be an unnamed list or `NULL`, not the number 1.
    Code
      normalize_turns(list(1))
    Condition
      Error in `normalize_turns()`:
      ! Every element of `turns` must be a `turn`.
    Code
      normalize_turns(list(sys_msg, user_msg), 1)
    Condition
      Error:
      ! `system_prompt` must be a character vector or `NULL`, not the number 1.
    Code
      normalize_turns(list(sys_msg, user_msg), "foo2")
    Condition
      Error:
      ! `system_prompt` and `turns[[1]]` can't contain conflicting system prompts.

