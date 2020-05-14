#' Removes outliers from a vector of numbers
#'
#' This function removes outliers using IQR +/- the 75th and 25th percentile. This is the same calculation as the boxplot() function.
#' @param x vector of numbers
#' @export
#' @examples
#' remove_outliers(x = vector)

remove_outliers <- function (x){
  quart_low <- quantile (x, na.rm = T)[[2]]
  quart_high <- quantile (x, na.rm = T)[[4]]
  outlier_dist <- IQR (x, na.rm = T) * 1.5
  
  low_thresh <- quart_low - outlier_dist
  high_thresh <- quart_high + outlier_dist
  
  x <- x [ which (x > low_thresh & x < high_thresh) ]
  
  return (x)
}