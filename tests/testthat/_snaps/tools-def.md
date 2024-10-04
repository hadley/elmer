# tool_arg checks its inputs

    Code
      tool_arg(1, letters[1:3], NA)
    Condition
      Error:
      ! <elmer::tool_arg> object properties are invalid:
      - @type must be <character>, not <double>
      - @description must be a single string, not a character vector.
      - @required must be a TRUE or FALSE, not NA.
    Code
      tool_def(1, letters[1:3], 1)
    Condition
      Error:
      ! <elmer::tool_def> object properties are invalid:
      - @name must be <character>, not <double>
      - @description must be a single string, not a character vector.
      - @arguments must be <list>, not <double>
    Code
      tool_def("", "", list(1))
    Condition
      Error:
      ! <elmer::tool_def> object properties are invalid:
      - @arguments must be a list of <turn>s. Element 1 is the number 1.
    Code
      tool_def("", "", list(tool_arg("", "")))
    Condition
      Error:
      ! <elmer::tool_def> object properties are invalid:
      - @arguments must be a named list.

