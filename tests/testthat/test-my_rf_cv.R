test_that("non-numeric input throws error", {
  expect_error(my_lm("a string"))
})
