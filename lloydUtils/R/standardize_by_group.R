#' Standardize a numeric vector based on group identity
#'
#' This function scales and centers a numeric vector within groups. Numbers from a single group are scaled linearly between 0 and 1, then centered by subtracting the scaled mean. This process is performed independently for all groups.
#' @param x numeric vector
#' @param groups vector with group labels
#' @return Returns a numeric vector standardized by group label.
#' @export
#' @examples
#' x <- 9:1
#' groups <- c("A", "B", "C", "A", "B", "C", "A", "B", "C" )
#' standardize_by_group(x = x, groups = groups)
#' > 0.5  0.5  0.5  0.0  0.0  0.0 -0.5 -0.5 -0.5

standardize_by_group <- function(x, groups){
  
  groups.uniq <- unique(groups)
  x.scale_center <- c()
  for(i in 1:length(groups.uniq)){
    group <- groups.uniq[i]
    group_i <- which(groups == group)
    
    group_vals <- x[group_i]
    group_vals.scale <- ( group_vals - min(group_vals) ) / ( max(group_vals) - min(group_vals) )
    group_vals.scale.center <- group_vals.scale - mean(group_vals.scale)
    
    x.scale_center[group_i] <- group_vals.scale.center
  }
  return(x.scale_center)
}