#' Consolidate multiple performance values
#'
#' This function will pull the performance metric and P-value of choice and consolidate these values into a single data-frame, with metrics across columns and different response vectors across rows.
#' @param perfObj object with performances from multiple response vectors ($performance_objects from multi_y.prediction_and_performance_with_validation_set.wrapper())
#' @param metric_i index with target metric: 1 = Pearson's r, 3 = Spearman's rho, 5 = median error, 6 = MSE, 7 = RSS
#' @param P_i index with target P-value: 2 = Pearson's p-value, 4 = Spearman's p-value
#' @return Returns a 4-column data frame: col1 = metric, col2 = p-value, col3 = adjusted p-value (FDR method), col4 = p-value factor. Data frame rows contain metrics for multiple response values.
#' @export
#' @examples
#' TBD

multi_y.consolidate_performances <- function(perfObj, metric_i, P_i){

  metric_vec <- sapply( X = 1:length(perfObj), FUN = function(i) perfObj[[i]][[metric_i]] )
  names(metric_vec) <- names(perfObj)

  P_vec <- sapply( X = 1:length(perfObj), FUN = function(i) perfObj[[i]][[P_i]] )
  names(P_vec) <- names(perfObj)

  P_vec.adj <- p.adjust(p = P_vec, method = "fdr")

  P_vec.class <- factor( ifelse( test = P_vec.adj < 0.05, yes = "p<0.05", no = ifelse(test = P_vec.adj >= 0.1, yes = "p>=0.1", no = "p<0.10") ),
                         levels = c("p<0.05", "p<0.10", "p>=0.1"))

  df.performances <- data.frame( metric = metric_vec, P = P_vec, P_adj = P_vec.adj, P_class = P_vec.class )

  metric_name <- names(perfObj[[1]][metric_i])
  names(df.performances)[1] <- metric_name
  return(df.performances)
}
