test_that("non-numeric input throws error", {
  expect_error(my_lm("a string"))
})

test_that("my_lm returns a list", {
  expect_is(my_t.test(x=(my_gapminder$lifeExp),alternative="two.sided",mu = 60),"list")
})
