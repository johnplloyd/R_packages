#' Scale and center a matrix
#'
#' This function will linearly scale the row or column values in a matrix between 0 and 1, and then subtract the scaled mean.
#' @param X matrix or data.frame
#' @param MARGIN scale and center row-wise (1; default) or column-wise (2)
#' @return Returns the scaled and centered matrix
#' @export
#' @examples
#' X <- matrix(data = 1:80, nrow = 10)
#' scale_center_matrix(X = X, MARGIN = 1)

scale_center_matrix <- function(X, MARGIN = 1){
  X.scale <- apply( X = X, MARGIN = MARGIN, FUN = function(x) ( x - min(x) )/( max(x) - min(x) )  )
  X.center <- apply( X = X.scale, MARGIN = MARGIN, FUN = function(x) x - mean(x)  )
  return(X.center)
}
