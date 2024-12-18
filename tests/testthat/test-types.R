test_that("can convert arrays of basic types to simple vectors", {
  expect_equal(
    convert_from_type(list(FALSE, TRUE), type_array(items = type_boolean())),
    c(FALSE, TRUE)
  )
  expect_identical(
    convert_from_type(list(1, 2), type_array(items = type_integer())),
    c(1L, 2L)
  )
  expect_equal(
    convert_from_type(list(1.2, 2.5), type_array(items = type_number())),
    c(1.2, 2.5)
  )
  expect_equal(
    convert_from_type(list("x", "y"), type_array(items = type_string())),
    c("x", "y")
  )
})

test_that("can covert array of arrays to lists of vectors", {
  expect_equal(
    convert_from_type(
       list(list(1, 2), list(3, 4)),
       type_array(items = type_array(items = type_integer()))
     ),
     list(c(1L, 2L), c(3L, 4L))
  )
})

test_that("can convert arrays of enums to factors", {
  expect_equal(
    convert_from_type(
      list("x", "y"),
      type_array(items = type_enum(values = c("x", "y", "z")))
    ),
    factor(c("x", "y"), levels = c("x", "y", "z"))
  )
})

test_that("can covert arrays of objects to data framnes", {
  expect_equal(
    convert_from_type(
      list(list(x = 1, y = "x"), list(x = 3, y = "y")),
      type_array(items = type_object(
        x = type_integer(),
        y = type_string()
      ))
    ),
    data.frame(x = c(1L, 3L), y = c("x", "y"))
  )
})

test_that("can recursive convert objets contents", {
  expect_equal(
    convert_from_type(
      list(x = 1, y = list(1, 2, 3)),
      type_object(
        x = type_integer(),
        y = type_array(items = type_integer())
      )
    ),
    list(x = 1, y = c(1, 2, 3))
  )
})
