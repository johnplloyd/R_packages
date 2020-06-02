#' Train, apply, and assess performance of prediction models developed with multiple response vectors and a hold-out validation set
#'
#' This function takes a matrix of response values and feature values and trains a model for each response columns based on the given feature set. Each model is then applied to a hold-out validation set.
#' @param model_type algorithm to train model with: "glmnet" or "plsr"
#' @param Y matrix with column-wise response values, training set
#' @param X matrix with column-wise feature values, training set
#' @param Y.val matrix with column-wise response values, validation set
#' @param X.val matrix with column-wise feature values, validation set
#' @param parameters_obj object with algorithm-specific parameters
#' @param n_rep model-building repetitions (i.e. randomly-subsetting training set): integer, default = 10
#' @param prop_per_rep proportion of training set to randomly-select for model training each repetition: [0, 1], default = 0.70
#' @param parallel Boolean indicating whether to perfom the model repetitions in parallel: default = FALSE
#' @return Returns a set of prediction objects (see train_and_apply_with_validation_set()) and a set of performance objects (see performance.regression()), one for each response vector.
#' @export
#' @examples
#' TBD
#'
multi_y.prediction_and_performance_with_validation_set.wrapper <- function(model_type, Y.train, X.train, Y.val, X.val, parameters_obj, n_rep = 10, prop_per_rep = 0.7, parallel = FALSE){

  if(parallel){
    predOutput <- foreach(i = 1:ncol(Y.train)) %dopar% {
      train_and_apply_with_validation_set(model_type = model_type,
                                          y = Y.train[,i], X = X.train,
                                          X.val = X.val,
                                          parameters_obj = parameters_obj,
                                          n_rep = n_rep, prop_per_rep = prop_per_rep)
    }
  }else{
    predOutput <- lapply(X = 1:ncol(Y.train), FUN = function(i)
      train_and_apply_with_validation_set(model_type = model_type,
                                          y = Y.train[,i], X = X.train,
                                          X.val = X.val,
                                          parameters_obj = parameters_obj,
                                          n_rep = n_rep, prop_per_rep = prop_per_rep)
      )
  }
  names(predOutput) <- names(Y.train)

  perfOutput <- lapply( X = 1:length(predOutput), FUN = function(i) performance.regression(o = Y.val[,i], p = predOutput[[i]]$pred_mean) )
  names(perfOutput) <- names(predOutput)

  return( list(prediction_objects = predOutput, performance_objects = perfOutput) )
}
