test_that("prop_whole_number validates inputs", {
  check_prop <- function(...) {
    new_class(
      "class",
      properties = list(prop = prop_number_whole(...)),
      package = NULL
    )
  }
  expect_snapshot(error = TRUE, {
    check_prop()("x")
    check_prop()(c(1:2))
    check_prop()(1.5)
    check_prop(min = 1)(0)
    check_prop(max = -1)(0)
  })
})
