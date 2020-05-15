#' Train and apply machine learning algorithms under cross-validation
#'
#' This function will hold out data by CV fold, train prediction model, and apply it to the held out fold. Acts a wrapper around train_and_apply_model().
#' @param model_type algorithm to use: "glment", "plsr"
#' @param y response vector
#' @param X feature matrix
#' @param n_fold number of CV folds: Integer
#' @param n_rep number of repetitions: Integer
#' @param parameters_obj object with algorithm-specific parameters
#' @return Returns an object with 5 items: 1) data frame with predicted value from each CV repetition for each instance ($df_pred), 2) object with reporter items, such as parameters selected, from each CV repetition and CV fold ($report_obj), 3) object with the CV fold assignments for each repetition ($CV_folds), 4) mean predicted value across repetitions for each instance ($pred_mean), and 5) variance in predicted values across reptitions for each instance ($pred_var)
#' @export

train_and_apply_under_CV <- function( model_type, y, X, n_fold, n_rep, parameters_obj ){

  x <- as.matrix(X)
  n_instances <- length(y)

  CV_folds_obj <- lapply(X = 1:n_rep, FUN = function(i) assign_CV_folds(n_instances = n_instances, n_fold = n_fold) )

  df_pred <- data.frame( row.names = rownames(X) )
  report_obj <- list()
  for( i in 1:n_rep ){
    print(c(i, "=== === === ==="))
    CV_folds_vec <- CV_folds_obj[[i]]
    CV_folds_vec

    pred_vec <- c()
    report_obj1 <- list()
    for(j in 1:n_fold){
      print(c("        ", j))
      testing_ind <- which(CV_folds_vec == j)
      training_ind <- which(CV_folds_vec != j)

      y.train <- y[training_ind]
      y.test <- y[testing_ind]

      X.train <- x[training_ind, , drop = F]
      X.test <- x[testing_ind, , drop = F]

      modelObj <- train_and_apply_model(model_type = model_type, y = y.train, X = X.train, newy = y.test, newx = X.test, parameters_obj = parameters_obj )

      pred_vec <- c(pred_vec, modelObj$predicted)
      report_obj1[[j]] <- modelObj$report_items
    }

    df_pred <- cbind(df_pred, pred_vec[row.names(df_pred)] )
    names(df_pred)[i] <- paste("rep", i, sep = "")

    names(report_obj1) <- paste("fold", 1:n_fold, sep = "")

    report_obj[[i]] <- report_obj1
    names(report_obj)[i] <- paste("rep", i, sep = "")
  }

  pred_mean <- rowMeans(df_pred)
  pred_var <- apply( X = df_pred, MARGIN = 1, FUN = var )

  returnObj <- list(df_pred = df_pred, report_obj = report_obj, CV_folds = CV_folds_obj, pred_mean = pred_mean, pred_var = pred_var )

  return( returnObj )
}
