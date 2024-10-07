test_that("ToolDef can get name", {
  f <- function() {}
  td <- ToolDef(f, description = "")
  expect_equal(td@name, "f")

  expect_snapshot(ToolDef(function() {}, description = ""), error = TRUE)
})

test_that("ToolArg checks its inputs", {
  expect_snapshot(error = TRUE, {
    ToolArg(1, letters[1:3], NA)
    ToolDef("", 1, letters[1:3], 1)
    ToolDef(identity, "", "", list(1))
    ToolDef(identity, "", "", list(ToolArg("", "")))
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
