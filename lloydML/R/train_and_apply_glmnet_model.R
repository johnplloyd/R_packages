#' Train and apply a regularized regression model
#'
#' This function will train a regularized regression model and apply it to new data using the 'glmnet' package. This function requires an object with specific parameters. Example parameter objects are attached to this package: param.LASSO, param.ElNet, param.ridge. If requested, lambda will be tuned under CV within the training set. Lambda values to test, CV folds, and CV repetitions are provided in the parameters object.
#' @param y_glm response vector - training
#' @param X_glm feature matrix - training
#' @param newx_glm feature matrix - testing
#' @param glmnet_parameters_obj object with glmnet-specific parameters
#' @return Returns an object with three items: 1) vector with predicted response values for the testing instances ($predicted), 2) lambda value selected under CV in the training set ($report_items), and 3) the glmnet model object ($model)
#' @export

train_and_apply_glmnet_model <- function(y_glm, X_glm, newx_glm, glmnet_parameters_obj){

  if (!requireNamespace("glmnet", quietly = TRUE)) {
    stop("\nPackage \"glmnet\" needed for train_and_apply_glmnet_model() to work.\nPlease install: install.packages(\"glmnet\")",
         call. = FALSE)
  }

  #0 = Ridge regression; 1 = LASSO; 0 < a < 1 = Elastic net

  X_glm <- as.matrix(X_glm)

  if(glmnet_parameters_obj$search_lambda){
    lambda <- iterate_search_lambda (y_glm = y_glm,
                                     X_glm = X_glm,
                                     glmnet_parameters_obj = glmnet_parameters_obj)
  }else{
    lambda <- glmnet_parameters_obj$lambda
  }

  #print(lambda)
  glmnet_model <- glmnet(X_glm, y_glm, alpha = glmnet_parameters_obj$alpha, lambda = lambda)
  glmnet_predicted <- predict(object = glmnet_model, newx = as.matrix(newx_glm), s = glmnet_parameters_obj$alpha)[,1]

  glmnet_obj <- list( predicted = glmnet_predicted, report_items = list(lambda = lambda), model = glmnet_model )

  return(glmnet_obj)
}
