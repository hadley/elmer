test_that("NULLS + values", {
  expect_equal(merge_dicts(list(), list(a = 1)), list(a = 1))
  expect_equal(merge_dicts(list(a = 1), list()), list(a = 1))
})

test_that("equal values are ok", {
  # Merge equal numeric values.
  expect_equal(merge_dicts(list(a = 1), list(a = 1)), list(a = 1))
  expect_equal(merge_dicts(list(a = 1.5), list(a = 1.5)), list(a = 1.5))
  expect_equal(
    merge_dicts(list(a = list(b = 1)), list(a = list(c = 2))),
    list(a = list(b = 1, c = 2))
  )

  # Merge equal logical values.
  expect_equal(merge_dicts(list(a = TRUE), list(a = TRUE)), list(a = TRUE))
  expect_equal(merge_dicts(list(a = FALSE), list(a = FALSE)), list(a = FALSE))

})

test_that("strings are concatenated", {
  expect_equal(merge_dicts(list(a = "txt"), list(a = "txt")), list(a = "txttxt"))
  expect_equal(
    merge_dicts(list(a = list(b = "txt")), list(a = list(b = "txt"))),
    list(a = list(b = "txttxt"))
  )
})

test_that("can merge dictionaries with different keys", {
  expect_equal(
    merge_dicts(list(a = 1, b = 2), list(a = 1)),
    list(a = 1, b = 2)
  )
  expect_equal(
    merge_dicts(list(a = 1, b = 2), list(c = NULL)),
    list(a = 1, b = 2, c = NULL)
  )
})

test_that("can merge lists", {
  expect_equal(
    merge_dicts(list(a = list(1, 2)), list()),
    list(a = list(1, 2))
  )
  expect_equal(
    merge_dicts(list(), list(a = list(1, 2))),
    list(a = list(1, 2))
  )
  expect_equal(
    merge_dicts(list(a = list(1, 2)), list(a = list(3))),
    list(a = list(1, 2, 3))
  )
})

test_that("respects index when merging lists", {
  expect_equal(
    merge_dicts(
      list(a = list(list(index = 0, b = "{"))),
      list(a = list(list(index = 0, b = "f")))
    ),
    list(a = list(list(index = 0, b = "{f")))
  )

  expect_equal(
    merge_dicts(
      list(a = list(list(index = 0, b = "a"))),
      list(a = list(list(index = 1, b = "b")))
    ),
    list(a = list(list(index = 0, b = "a"), list(index = 1, b = "b")))
  )
})
