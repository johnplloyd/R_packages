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

train_and_apply_with_validation_set <- function( model_type, y, X, X.val, n_rep, prop_per_rep, parameters_obj ){

  # model_type <- "glmnet"
  # Y1 <- m.AUC.GDSC[1]
  # X1 <- m.RNA.GDSC.common
  # X2 <- m.RNA.TNBC.common
  # parameters_obj <- glmnet_parameters_obj.LASSO
  # n_rep <- 5
  # prop_per_rep <- 0.25

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
  #random_inds

  df_pred <- data.frame( row.names = rownames(X2) )
  report_obj <- list()
  for(i in 1:n_rep){
    print(c(i, "=== === ==="))
    bagging_ind <- bagging_inds[[i]]
    y1.train <- y1[bagging_ind]
    X1.train <- X1[bagging_ind,]

    # length(y1.train)
    # dim(X1)
    # dim(X1.train)
    # dim(X2)
    # corner(X1)

    modelObj <- train_and_apply_model(model_type = model_type, y = y1.train, X = X1.train,
                                      newy = NULL, newx = X2,
                                      parameters_obj = parameters_obj )
    df_pred <- cbind(df_pred, modelObj$predicted)
    names(df_pred)[i] <- paste("rep", i, sep = "")
    #df_pred

    report_obj[[i]] <- modelObj$report_items
    names(report_obj)[i] <- paste("rep", i, sep = "")
    #report_obj
  }

  #head(df_pred)
  #report_obj

  pred_mean <- rowMeans(df_pred)
  #pred_mean

  pred_var <- apply( X = df_pred, MARGIN = 1, FUN = var )
  #pred_var

  returnObj <- list(df_pred = df_pred, training_inds = random_inds, report_obj = report_obj, pred_mean = pred_mean, pred_var = pred_var )
  return(returnObj)
}
