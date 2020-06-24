#' Standardize a numeric vector based on group identity
#'
#' This function scales and centers a numeric vector within groups. Numbers from a single group are scaled linearly between 0 and 1, then centered by subtracting the scaled mean. This process is performed independently for all groups.
#' @param x numeric vector
#' @param group_labels vector with group labels
#' @return Returns a numeric vector standardized by group label.
#' @export
#' @examples
#' x <- 9:1
#' group_labels <- c("A", "B", "C", "A", "B", "C", "A", "B", "C" )
#' standardize_by_group(x = x, group_labels = group_labels)
#' > 0.5  0.5  0.5  0.0  0.0  0.0 -0.5 -0.5 -0.5

standardize_by_group <- function(x, group_labels){
  
  na.rm <- T
  
  groups.uniq <- unique(group_labels)
  x.scale_center <- c()
  for(i in 1:length(groups.uniq)){
    group <- groups.uniq[i]
    group_i <- which(group_labels == group)
    
    group_vals <- x[group_i]
    group_vals.scale <- ( group_vals - min(group_vals, na.rm = na.rm) ) / ( max(group_vals, na.rm = na.rm) - min(group_vals, na.rm = na.rm) )
    group_vals.scale.center <- group_vals.scale - mean(group_vals.scale, na.rm = na.rm)
    
    x.scale_center[group_i] <- group_vals.scale.center
  }
  return(x.scale_center)
}
