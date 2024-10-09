# checks inputs

    Code
      prompt(1)
    Condition
      Error in `prompt()`:
      ! `prompt` must be a single string, not the number 1.
    Code
      prompt("x", 1)
    Condition
      Error in `prompt()`:
      ! All elements of `...` must be named
    Code
      prompt("{{x}}", x = 1:2)
    Condition
      Error in `prompt()`:
      ! Must generate a single string.
      i Did you accidentally include a vector in `...``?

