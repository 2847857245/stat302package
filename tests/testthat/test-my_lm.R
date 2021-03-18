test_that("non-numeric input throws error", {
  expect_error(my_lm("a string"))
})

test_that("my_lm returns numeric Estimate", {
  expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder)$Estimate, "numeric")
})

test_that("my_lm returns a data frame", {
  expect_is(my_lm(lifeExp ~ gdpPercap + continent, data = my_gapminder),"data.frame")
})

