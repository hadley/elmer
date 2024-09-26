test_that("call tool gives useful error", {
  expect_snapshot(call_tool("foo", c(1, 2, 3)))
})
