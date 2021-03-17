#' Linear regression
#'
#' This functions gives a t test based on the input
#'
#' @param formula a formula class object, similar to lm().
#' @param data Input data frame.
#'
#' @keywords \code{prediction}
#'
#' @return A table similar to the coefficent table from summary() with rows for each coefficient
#'   and columns for the Estimate, Std. Error, t value,
#'   and Pr(>|t|).
#'
#' @examples
#' my_lm (mpg ~ hp + wt, data = mtcars)
#'
#'
#' @export
my_lm <- function(formula, data) {
  # Extract matrix X an Y from the parameters
  x <- model.matrix(formula, data)
  y <- model.response(model.frame(formula, data))
  beta <- solve(t(x) %*% x) %*% t(x) %*% y

  #Calculate the degree of freedom, variance and standard error
  df <- nrow(x) - ncol(x)
  variance <- sum((y - x %*% beta)^2 / df)
  se <- sqrt(diag(variance * solve(t(x) %*% x)))

  #Calculate thet stats and the corresponding p value
  t_stats <- beta / se
  p_value <- 2 * pt(abs(t_stats), df, lower.tail = FALSE)

  #define the output as similar to the summary of lm
  #including estimate, standard error, t value and p-value
  result <- list("Estimate" = beta,
                 "Std. Error" = se,
                 "t value" = t_stats,
                 "Pr(>|t|)" = p_value)
  return(data.frame(result))
}
