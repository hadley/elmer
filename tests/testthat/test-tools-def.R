test_that("tool can get name", {
  f <- function() {}
  td <- tool(f, "")
  expect_equal(td@name, "f")

  td <- tool(function() {}, "")
  expect_match(td@name, "^tool_")
})

test_that("json_schema_parameters generates correct paramters if no arguments", {
  expect_equal(
    as_json(Provider(""), type_object()),
    list(
      type = "object",
      description = "",
      properties = set_names(list()),
      required = list(),
      additionalProperties = FALSE
    )
  )
})
