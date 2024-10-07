# ToolArg checks its inputs

    Code
      ToolArg(1, letters[1:3], NA)
    Condition
      Error:
      ! <elmer::ToolArg> object properties are invalid:
      - @type must be <character>, not <double>
      - @description must be a single string, not a character vector.
      - @required must be a TRUE or FALSE, not NA.
    Code
      ToolDef(1, letters[1:3], 1)
    Condition
      Error:
      ! <elmer::ToolDef> object properties are invalid:
      - @name must be <character>, not <double>
      - @description must be a single string, not a character vector.
      - @arguments must be <list>, not <double>
    Code
      ToolDef("", "", list(1))
    Condition
      Error:
      ! <elmer::ToolDef> object properties are invalid:
      - @arguments must be a list of <ToolArg>s. Element 1 is the number 1.
    Code
      ToolDef("", "", list(ToolArg("", "")))
    Condition
      Error:
      ! <elmer::ToolDef> object properties are invalid:
      - @arguments must be a named list.

