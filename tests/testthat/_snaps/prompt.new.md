# checks inputs

    Code
      interpolate(1)
    Condition
      Error in `interpolate()`:
      ! `prompt` must be a single string, not the number 1.
    Code
      interpolate("x", 1)
    Condition
      Error in `interpolate()`:
      ! All elements of `...` must be named
    Code
      interpolate("{{x}}", x = 1:2)
    Condition
      Error in `interpolate()`:
      ! Must generate a single string.
      i Did you accidentally include a vector in `...``?

