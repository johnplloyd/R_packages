#' Calculates median error between two vectors
#'
#' This function takes two vectors and calculates their median error, i.e. median of the absolute difference. Vectors must be of equal length. Expected usage is that one vector is observed values and the second vector is predicted values.
#' @param x1 numeric vector 1
#' @param x2 numeric vector 2
#' @return median error value
#' @export
#' @examples
#' x <- runif(5)
#' y <- runif(5)
#' calc_median_error(x1 = x, x2 = y)
#' [1] 0.2433127

calc_median_error <- function (x1, x2){
  diff <- x1 - x2
  abs_diff <- abs(diff)
  median_error <- median(abs_diff)
  return (median_error)
}
