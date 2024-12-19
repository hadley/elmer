test_that("invoke_tool returns a tool_result", {
  res <- invoke_tool(function() 1, list(), id = "x")
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@id, "x")
  expect_equal(res@error, NULL)
  expect_equal(res@value, 1)

  res <- invoke_tool(function() 1, list(x = 1), id = "x")
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@id, "x")
  expect_equal(res@error, "unused argument (x = 1)")
  expect_equal(res@value, NULL)

  res <- invoke_tool(NULL, list(x = 1), id = "x")
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@id, "x")
  expect_equal(res@error, "Unknown tool")
  expect_equal(res@value, NULL)
})

test_that("invoke_tool_async returns a tool_result", {
  res <- sync(invoke_tool_async(function() 1, list(), id = "x"))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@id, "x")
  expect_equal(res@error, NULL)
  expect_equal(res@value, 1)

  res <- sync(invoke_tool_async(function() 1, list(x = 1), id = "x"))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@id, "x")
  expect_equal(res@error, "unused argument (x = 1)")
  expect_equal(res@value, NULL)

  res <- sync(invoke_tool_async(NULL, list(x = 1), id = "x"))
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@id, "x")
  expect_equal(res@error, "Unknown tool")
  expect_equal(res@value, NULL)
})
