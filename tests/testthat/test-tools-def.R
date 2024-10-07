test_that("ToolArg checks its inputs", {
  expect_snapshot(error = TRUE, {
    ToolArg(1, letters[1:3], NA)
    ToolDef(1, letters[1:3], 1)
    ToolDef("", "", list(1))
    ToolDef("", "", list(ToolArg("", "")))
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
