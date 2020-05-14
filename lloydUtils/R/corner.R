#' Print the corner of a matrix
#'
#' This function prints a corner of a matrix. Corner to be printed is assigned with the 'c' parameter (default = top left).
#' @param X matrix or data.frame
#' @param n num of columns and rows to print
#' @param c matrix corner to print: "topleft" (default), "topright", "bottomleft", and "bottomright"
#' @return Prints the corner of a matrix
#' @export
#' @examples
#' X <- matrix(data = 1:80, nrow = 10)
#'
#' corner(X = X, n = 2, c = "topleft")
#' >      [,1] [,2]
#' > [1,]    1   11
#' > [2,]    2   12
#'
#' corner(X = X, n = 3, c = "bottomright")
#' >      [,1] [,2] [,3]
#' > [1,]   58   68   78
#' > [2,]   59   69   79
#' > [3,]   60   70   80

corner <- function(X, n = 6, c = "topleft"){
  if(tolower(c) == "topleft" | tolower(c) == "tl"){
    print(X[1:n, 1:n])
  }else if(tolower(c) == "topright" | tolower(c) == "tr"){
    print(X[1:n, (ncol(X)-n+1):ncol(X)])
  }else if(tolower(c) == "bottomleft" | tolower(c) == "bl"){
    print(X[(nrow(X)-n+1):nrow(X), 1:n])
  }else if(tolower(c) == "bottomright" | tolower(c) == "br"){
    print(X[(nrow(X)-n+1):nrow(X), (ncol(X)-n+1):ncol(X)])
  }
}
