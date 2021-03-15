#' T test method
#'
#' This functions gives a t test based on the input
#'
#' @param x Numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis.
#'   This should only accept `"two.sided"`, `"less"`, or `"greater"`.
#'   Otherwise, this function will throw an informative error.
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @return A list with elements including the numeric test statistic \code{t_test},
#'   the degrees of freedom \code{df}, the value of the parameter \code{alternative},
#'   and the the numeric p-value \code{p_val}.
#'
#' @example
#' my_t.test(x = c(3.0,3.1,3.2,2.9,2.9,3.0), alternative = less, mu = 3)
#'
#' @export
my_t.test <- function(x,alternative,mu){
  ## get the sd of sampling distribution
  se <- sd(x)/sqrt(length(x))
  test_stat <- (mean(x) - mu)/se
  df <- length(x) - 1

  ## Use control flow to deal with different alternative values
  if (alternative == "two.sided") {
    p_val <- 2* pt(abs(test_stat), df, lower.tail=FALSE)
  } else if (alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else {
    stop("The value of alternative is undefined")
  }

  ## Create the result list
  result <- list("test_stat" = test_stat,
                 "df" = df,
                 "p_value" = p_val,
                 "alternative" = alternative)
  return(result)
}
