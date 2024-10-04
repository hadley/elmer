test_that("tool_arg checks its inputs", {
  expect_snapshot(error = TRUE, {
    tool_arg(1, letters[1:3], NA)
    tool_def(1, letters[1:3], 1)
    tool_def("", "", list(1))
    tool_def("", "", list(tool_arg("", "")))
  })
})

test_that("json_schema_parameters generates correct paramters if no arguments", {
  expect_equal(
    json_schema_parameters(list()),
    list(
      type = "object",
      properties = set_names(list()),
      required = list(),
      additionalProperties = FALSE
    )
  )
})
