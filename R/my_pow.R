#' Power function
#'
#' This functions raises input to a power.
#'
#' @param x Numeric input to be raised to the power of \code{power}.
#' @param power Numeric input for the power that \code{x} will be raised to,
#'   default to \code{2}.
#'
#' @return Numeric representing \code{x} raised to the power of \code{power}.
#'
#' @examples
#' my_pow(4)
#' my_pow(4, power = 3)
#'
#' @export
my_pow <- function(x, power = 2) {
  return(x^power)
}
