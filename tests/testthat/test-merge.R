test_that("NULLS + values", {
  # Merge `None` and `1`.
  expect_equal(merge_dicts(list(a = NULL), list(a = 1)), list(a = 1))

  # Merge `1` and `None`.
  expect_equal(merge_dicts(list(a = 1), list(a = NULL)), list(a = 1))

  # Merge `None` and a value.
  expect_equal(merge_dicts(list(a = NULL), list(a = 0)), list(a = 0))
  expect_equal(merge_dicts(list(a = NULL), list(a = "txt")), list(a = "txt"))
})

test_that("equal values", {
  # Merge equal numeric values.
  expect_equal(merge_dicts(list(a = 1), list(a = 1)), list(a = 1))
  expect_equal(merge_dicts(list(a = 1.5), list(a = 1.5)), list(a = 1.5))

  # Merge equal logical values.
  expect_equal(merge_dicts(list(a = TRUE), list(a = TRUE)), list(a = TRUE))
  expect_equal(merge_dicts(list(a = FALSE), list(a = FALSE)), list(a = FALSE))

})

test_that("string and list values", {

  # Merge strings.
  expect_equal(merge_dicts(list(a = "txt"), list(a = "txt")), list(a = "txttxt"))
  expect_equal(merge_dicts(list(a = "one"), list(a = "two")), list(a = "onetwo"))

  # Merge lists.
  expect_equal(merge_dicts(list(a = list(1, 2)), list(a = list(1, 2))), list(a = list(1, 2, 1, 2)))
  expect_equal(merge_dicts(list(a = list(1, 2)), list(a = list(3))), list(a = list(1, 2, 3)))

})

test_that("Merging dictionaries with nested dictionaries", {

  # Merge nested dictionaries with the same keys.
  expect_equal(merge_dicts(list(a = list(b = "txt")), list(a = list(b = "txt"))), list(a = list(b = "txttxt")))

  # Merge nested dictionaries with different keys.
  expect_equal(merge_dicts(list(a = list(b = 1)), list(a = list(c = 2))), list(a = list(b = 1, c = 2)))

})

test_that("Merging dictionaries with different keys", {

  # Merging with different keys in the dictionaries.
  expect_equal(merge_dicts(list(a = 1, b = 2), list(a = 1)), list(a = 1, b = 2))
  expect_equal(merge_dicts(list(a = 1, b = 2), list(c = NULL)), list(a = 1, b = 2, c = NULL))

})

test_that("Merging complex nested dictionaries", {

  # Merge nested dictionaries with None and strings.
  expect_equal(
    merge_dicts(list(function_call = list(arguments = NULL)), list(function_call = list(arguments = "{\n"))),
    list(function_call = list(arguments = "{\n"))
  )

})
