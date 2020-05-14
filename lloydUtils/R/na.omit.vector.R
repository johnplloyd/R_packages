#' Removes NAs from a vector
#'
#' This function removes NA values from a vector
#' @param x vector
#' @return Prints the topleft corner of a matrix
#' @export
#' @examples
#' na.omit.vector(x = vector)

na.omit.vector <- function(x){
  x.omit <- x[ !(is.na(x)) ]
  return(x.omit)
}