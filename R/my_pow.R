#' Power function
#'
#' This functions raises input to a power.
#'
#' @param x Numeric input to be raised to the power of \code{power}.
#' @param power Numeric input for the power that \codee{x} will be raised to,
#'   default to \code{2}.
#'
#' @return Numeric representing \code{x} raised to the power of \code{power}.
#'
#' @example
#' my_power(4)
#' my_power(4, power = 3)
#'
#' @export
my_pow <- function(x, power = 2) {
  return(x^power)
}
