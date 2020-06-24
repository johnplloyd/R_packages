#' Calculate the variantion in a numeric vector explained by group identity
#'
#' This function calculates the variation in a vector that is explained by group labels using lm() and anova().
#' @param x numeric vector
#' @param group_labels vector with group labels
#' @return Returns the proportion of variation explained by provided group labels.
#' @export
#' @examples
#' x <- 9:1
#' group_labels <- c("A", "B", "C", "A", "B", "C", "A", "B", "C" )
#' variation_by_group(x = x, group_labels = group_labels)

variation_by_group <- function(x, group_labels){
  
  lm1 <- lm( x ~ group_labels )
  anova1 <- anova(lm1)
  var_expl <- anova1$`Sum Sq`[1] / sum(anova1$`Sum Sq`)
  
  return(var_expl)
}
