#' Calculate performance metrics for a regression model
#'
#' This function takes vectors of observed (o) and predicted (p) values and calculates 7 performance metrics: Pearson's r (+ P-value), Spearman's rho (+ P-value), median error, mean squared error, and residual sum of squares.
#' @param o numeric vector with observed values
#' @param p numeric vector with predicted values
#' @return object with 7 performance metrics: $r, $r.p, $rho, $rho.p, $median_error, $MSE, $RSS
#' @export
#' @examples
#' x <- runif(5)
#' y <- runif(5)

multi_y.prediction_and_performance.wrapper <- function(pred_obj, m_Y, metric_i, P_i, metric_name){

  # pred_obj <- output.LASSO.varExpr_subset
  # m_obs <- m.AUC
  # metric_i <- 3
  # P_i <- 4
  # metric_name <- "rho"

  perf_obj <- lapply( X = 1:length(pred_obj), FUN = function(i) performance.obs_v_pred(o = m_obs[,i], p = pred_obj[[i]]$pred_mean) )
  names(perf_obj) <- colnames(m_obs)
  df_perf <- select_performances(perf_obj = perf_obj, metric_i = metric_i, P_i = P_i, metric_name = metric_name)
  return( list(perf_obj = perf_obj, df_perf = df_perf) )
}
