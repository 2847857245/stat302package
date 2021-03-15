test_that("my_pow works mathematically", {
  expect_equal(my_pow(2), 4)
  expect_equal(my_pow(2, power = 3), 8)
})
test_that("non-numeric input throws error", {
  expect_error(my_pow("a string"))
  expect_error(my_pow(2, power = "a string"))
})
