test_that("useful message if no tokens", {
  tokens_reset()
  expect_snapshot(token_usage())
})

test_that("can retrieve and log tokens", {
  defer(tokens_reset())

  tokens_log("testing", c(10, 50))
  tokens_log("testing", c(0, 10))

  df <- token_usage()
  expect_equal(df$input[df$name == "testing"], 10)
  expect_equal(df$output[df$name == "testing"], 60)
})
