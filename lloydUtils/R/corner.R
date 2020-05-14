#' Prints the top left corner of a matrix
#'
#' This function prints the topleft corner of a matrix.
#' @param X matrix or data.frame
#' @param n num of columns and rows to print
#' @return Prints the topleft corner of a matrix
#' @export
#' @examples
#' corner(X = matrix)

corner <- function(X, n = 10){
  print(m[1:n, 1:n])
}