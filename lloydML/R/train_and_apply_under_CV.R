#' Train and apply machine learning algorithms under cross-validation
#'
#' This function will 1) remove instances with NA response values, 2) assign CV folds randomly, 3) iterively subset into training/testing data based on CV folds, and 4) send training/testing data to model training functions. Acts a wrapper around train_and_apply_model().
#' @param model_type algorithm: "glmnet", "plsr"
#' @param y response vector
#' @param X feature matrix
#' @param n_fold number of CV folds: Integer
#' @param parameters_obj object with algorithm-specific parameters
#' @return Returns an object with 2 items: 1) vector with predicted response values ($predicted) and 2) object with reporter items, such as parameters selected, from each CV repetition and CV fold ($report_obj)
#' @export

train_and_apply_under_CV <- function( model_type, y, X, n_fold, parameters_obj ){

  # 1) remove instances with NA response values
  if(anyNA(y)){
    NA_ind <- which(is.na(y))
    y <- y[-NA_ind]
    X <- X[-NA_ind,]
  }

  X <- as.matrix(X)
  n_instances <- length(y)

  # 2) assign CV folds randomly
  CV_folds <- assign_CV_folds(n_instances = n_instances, n_fold = n_fold)

  # Iterate through each fold
  pred_vec <- c()
  report_obj <- list()
  for(i in 1:n_fold){
    print(paste("          fold ", i, sep = ""))

    # 3) subset into training/testing data based on CV folds
    testing_ind <- which(CV_folds_vec == i)
    training_ind <- which(CV_folds_vec != i)

    y.train <- y[training_ind]
    y.test <- y[testing_ind]

    X.train <- X[training_ind, , drop = F]
    X.test <- X[testing_ind, , drop = F]

    # 4) send training/testing data to model training functions
    modelObj <- train_and_apply_model(model_type = model_type, y = y.train, X = X.train, newy = y.test, newx = X.test, parameters_obj = parameters_obj )

    pred_vec <- c(pred_vec, modelObj$predicted) # Add fold predictions to a single vector
    report_obj[[j]] <- modelObj$report_items # Store reporter items, such as tuned parameters
  }

  pred_vec <- pred_vec[row.names(X)]
  names(report_obj) <- paste("fold", 1:n_fold, sep = "")

  returnObj <- list(predicted = pred_vec, report_obj = report_obj)

  return( returnObj )
}
