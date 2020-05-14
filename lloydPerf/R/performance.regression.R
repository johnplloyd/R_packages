#' Calculates performance metrics for a regression model
#'
#' This function takes vectors of observed (o) and predicted (p) values and calculates 7 performance metrics: Pearson's r (+ P-value), Spearman's rho (+ P-value), median error, mean squared error, and residual sum of squares.
#' @param o numeric vector with observed values
#' @param p numeric vector with predicted values
#' @return object with 7 performance metrics: $r, $r.p, $rho, $rho.p, $median_error, $MSE, $RSS
#' @export
#' @examples
#' x <- runif(5)
#' y <- runif(5)
#' performance.regression(o = x, p = y)
#' $r
#' cor
#' 0.2370634
#'
#' $r.p
#' [1] 0.701013
#'
#' $rho
#' rho
#' 0.4
#'
#' $rho.p
#' [1] 0.5166667
#'
#' $median_error
#' [1] 0.2336084
#'
#' $MSE
#' [1] 0.5548035
#'
#' $RSS
#' [1] 0.5548035

performance.regression <- function(o, p){

  #o <- obs_vec
  #p <- pred_vec

  r_obj <- cor.test( x = o, y = p, method = "pearson" )
  rho_obj <- cor.test( x = o, y = p, method = "spearman" )
  ME <- calc_median_error(x1 = o, x2 = p)
  MSE <- calc_MSE(x1 = o, x2 = p)
  RSS <- calc_RSS(x1 = o, x2 = p)

  perf_obj <- list(r = r_obj$estimate, r.p = r_obj$p.value,
                   rho = rho_obj$estimate, rho.p = rho_obj$p.value,
                   median_error = ME, MSE = MSE, RSS = RSS)
  return(perf_obj)
}
