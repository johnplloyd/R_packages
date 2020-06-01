#' Select an optimal lambda value under multiple cross-validation rounds
#'
#' This function performs cross-validation runs to select an optimal lambda value. Performance metric is minimum mean squared error. Multiple CV runs are performed and the final lambda value is selected based on mode.
#' @param y_glm numeric vector with response values
#' @param X_glm matrix with features in columns
#' @param glmnet_parameters_obj glmnet paramteres object, see "param.LASSO" object loaded with lloydML
#' @return lamda selected most frequently under cross-validation
#' @export
#' @examples
#' lambda <- iterate_search_lambda(y, X, param)

iterate_search_lambda <- function(y_glm, X_glm, glmnet_parameters_obj){

  glmnet_parameters_obj
  alpha <- glmnet_parameters_obj$alpha
  search_lambdas <- glmnet_parameters_obj$search_lambdas
  n_search_folds <- glmnet_parameters_obj$n_search_folds
  n_search_rep <- glmnet_parameters_obj$n_search_rep

  lambda_selected <- c()
  for(i in 1:n_search_rep){
    cv_fit <- cv.glmnet(X_glm, y_glm, type = "mse", alpha = alpha, lambda = search_lambdas, nfolds = n_search_folds)
    nzero_lambda.ind <- which(cv_fit$nzero != 0)
    min_index <- which(cv_fit$cvm == min(cv_fit$cvm[nzero_lambda.ind]))
    lambda <- cv_fit$lambda[min_index]
    lambda_selected <- c(lambda_selected, lambda)
  }

  lambda_table <- table(lambda_selected)
  lambda_mode <- as.numeric(names(lambda_table[which(lambda_table == max(lambda_table))])[1])

  return(lambda_mode)
}
