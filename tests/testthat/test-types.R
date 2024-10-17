test_that("can convert basic types to json schema", {
  stub <- Provider("")

  expect_equal(
    as_json_schema(stub, type_boolean("desc")),
    list(type = "boolean", description = "desc")
  )

  expect_equal(
    as_json_schema(stub, type_enum("desc", letters[1:3])),
    list(type = "string", description = "desc", enum = as.list(letters[1:3]))
  )

  expect_equal(
    as_json_schema(stub, type_array("a", type_boolean("b"))),
    list(
      type = "array",
      description = "a",
      items = list(type = "boolean", description = "b")
    )
  )
})

test_that("can convert an object to json schema", {
  stub <- Provider("")
  obj <- type_object(
    "a",
    integer = type_integer(),
    number = type_number(),
    string = type_string()
  )

  expect_equal(
    as_json_schema(stub, obj),
    list(
      type = "object",
      description = "a",
       properties = list(
         integer = list(type = "integer", description = ""),
         number = list(type = "number", description = ""),
         string = list(type = "string", description = "")
       ),
       required = list("integer", "number", "string"),
       additionalProperties = FALSE
    )
  )
})

test_that("as_json_schema specialised for OpenAI", {
  stub <- ProviderOpenAI(base_url = "", api_key = "", model = "")

  expect_snapshot(
    as_json_schema(stub, type_object(.additional_properties = TRUE)),
    error = TRUE
  )

  obj <- type_object(x = type_number(required = FALSE))
  expect_equal(
    as_json_schema(stub, obj),
    list(
      type = "object",
      description = "",
      properties = list(x = list(type = c("number", "null"), description = "")),
      required = list("x"),
      additionalProperties = FALSE
    )
  )
})
