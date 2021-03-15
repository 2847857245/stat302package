test_that("f_to_c works mathematically", {
  expect_equal(f_to_c(32), 0)
  expect_equal(f_to_c(212), 100)
})
test_that("non-numeric input throws error", {
  expect_error(f_to_c("a string"))
})
