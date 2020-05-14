#' Calculates mean squared error (MSE) between two vectors
#'
#' This function takes two vectors and calculates their mean squared error. Vectors must be of equal length. Expected usage is that one vector is observed values and the second vector is predicted values.
#' @param x1 numeric vector 1
#' @param x2 numeric vector 2
#' @return MSE value
#' @export
#' @examples
#' x <- runif(5)
#' y <- runif(5)
#' calc_MSE(x1 = x, x2 = y)
#' [1] 0.7402145

calc_MSE <- function(x1, x2){
  rsdl <- x1-x2
  rsdl_sqr <- rsdl^2
  rss <- sum(rsdl_sqr)
  MSE <- rss/length(rss)
  return ( MSE )
}
