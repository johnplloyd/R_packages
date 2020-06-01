#' Train and apply machine learning algorithms with a validation set
#'
#' This function will randomly select a subset of the training data, train a prediction model, and apply it to the validation set. Acts a wrapper around train_and_apply_model().
#' @param model_type algorithm to use: "glmnet", "plsr"
#' @param y response vector: training set
#' @param X feature matrix: training set
#' @param X.val feature matrix: valiation set
#' @param n_rep number of repetitions: Integer
#' @param prop_per_rep proportion of training set to randomly select per repetition: [0, 1]
#' @param parameters_obj object with algorithm-specific parameters
#' @return Returns an object with 5 items: 1) data frame with predicted value from each bagging repetition for each instance in the validation set ($df_pred), 2) object with indices of training subset selected for each repetition ($training_inds), 3) object with reporter items, such as parameters selected, from each repetition ($report_obj), 4) mean predicted value across repetitions for validation set instances ($pred_mean), and 5) standard deviation of predicted values across reptitions for validation set instances ($pred_sd)
#' @export

train_and_apply_with_validation_set <- function( model_type, y, X, X.val, n_rep, prop_per_rep, parameters_obj ){

  if(anyNA(y)){
    NA_ind <- which(is.na(y))
    y <- y[-NA_ind]
    X <- X[-NA_ind,]
  }

  X <- as.matrix(X)
  X.val <- as.matrix(X.val)

  n_instances <- length(y)
  n_per_rep <- floor( n_instances * prop_per_rep )
  bagging_inds <- lapply(X = 1:n_rep, FUN = function(x) sort(sample( 1:n_instances, n_per_rep )) )
  names(bagging_inds) <- paste("rep", 1:n_rep, sep = "")

  df_pred <- data.frame( row.names = rownames(X.val) )
  report_obj <- list()
  for(i in 1:n_rep){
    print(paste("REPETITION", i))
    bagging_ind <- bagging_inds[[i]]
    y.train <- y[bagging_ind]
    X.train <- X[bagging_ind,]

    modelObj <- train_and_apply_model(model_type = model_type, y = y.train, X = X.train,
                                      newy = NULL, newx = X.val,
                                      parameters_obj = parameters_obj )
    df_pred <- cbind(df_pred, modelObj$predicted)
    names(df_pred)[i] <- paste("rep", i, sep = "")

    report_obj[[i]] <- modelObj$report_items
    names(report_obj)[i] <- paste("rep", i, sep = "")
  }

  pred_mean <- rowMeans(df_pred)
  pred_sd <- apply( X = df_pred, MARGIN = 1, FUN = sd )

  returnObj <- list(df_pred = df_pred, training_inds = bagging_inds, report_obj = report_obj, pred_mean = pred_mean, pred_sd = pred_sd )
  return(returnObj)
}
