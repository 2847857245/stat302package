test_that("non-numeric input throws error", {
  expect_error(my_lm("a string"))
})

test_that("my_rf_cv returns a numeric Estimate", {
  expect_is(my_rf_cv(k = 5),"numeric")
})
