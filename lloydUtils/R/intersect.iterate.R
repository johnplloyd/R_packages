#' Performs iterative intersect comparisons to find the intersect of >2 vectors.
#'
#' This function will find the items in common between a set of >2 vectors.
#' @param list_of_vectors list object containing the target vectors
#' @return vector of the items in common between all vectors in list_of_vectors
#' @export
#' @examples
#' X <- list( LETTERS[1:15], LETTERS[3:18], LETTERS[6:23] )
#' intersect.iterate(X)

intersect.iterate <- function( list_of_vectors ){
  
  # collect all items
  all_items <- c()
  for(i in 1:length(list_of_vectors)){
    vec <- list_of_vectors[[i]]
    head(vec)
    length(vec)
    all_items <- unique( c( all_items, vec ) )
  }
  all_items
  length(all_items)
  
  # iterate intersect
  intersect.all <- all_items
  for(i in 1:length(list_of_vectors)){
    vec <- list_of_vectors[[i]]
    head(vec)
    length(vec)
    intersect.all <- intersect(intersect.all, vec)
  }
  intersect.all
  return(intersect.all)
}