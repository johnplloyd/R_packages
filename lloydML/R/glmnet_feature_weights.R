#' Generate feature weights from glmnet models
#'
#' This function generates feature weights from glmnet models. Weights are produced from multiple iterations with subsets of training data.
#' @param y numeric response vector
#' @param X matrix or data frame with numeric feature values in columns, instances in rows
#' @param alpha glmnet model alpha. [0,1] 1 = LASSO (default), 0 = ridge regression, >0 & <1 = elastic net
#' @param lambda glmnet model lambda. default = 0.01.
#' @param n_rep Num of repetitions. Integer, default = 30
#' @param train_prop Proportion of training set to subsample for each repetition. [0,1], default = 0.7
#' @return Returns an object with three items: 1) mean feature weight across all repetitions, 2) data frame with the feature weight from each repetition, and 3) indices of instances selected for each repetition.
#' @export
#' @examples
#' glmnet_feature_weights(y = y, X = X, alpha = 1, lambda = 0.01, n_rep = 30, train_prop = 0.7)

glmnet_feature_weights <- function (y, X, alpha = 1, lambda = 0.01, n_rep = 30, train_prop = 0.7){

  library(glmnet)

  if(anyNA(y)){
    NA_ind <- which(is.na(y))
    y <- y[-NA_ind]
    X <- as.matrix(X[-NA_ind,])
  }

  n_instances <- length(y)
  n_train <- floor(n_instances * train_prop)

  training_instances <- lapply(X = 1:n_rep, FUN = function(x) sort(sample( 1:n_instances, n_train )))
  names(training_instances) <- paste("rep", 1:n_rep, sep = "")

  df_weights <- data.frame(row.names = colnames(X))
  for(i in 1:n_rep){

    # pull training instances
    train_ind <- training_instances[[i]]
    y_train <- y[train_ind]
    X_train <- X[train_ind,]

    # train model and extract coefficients
    glmnet_model <- glmnet(x = X_train, y = y_train, alpha = alpha, lambda = lambda)
    m.glmnet_coef <- predict(object = glmnet_model, type = 'coefficients', s = lambda)

    # convert single-column coefficients matrix to a vector
    glmnet_coef <- m.glmnet_coef[,1]
    names(glmnet_coef) <- rownames(m.glmnet_coef)

    # remove intercept weight
    glmnet_coef <- glmnet_coef[-1]

    df_weights <- cbind(df_weights, glmnet_coef)
    names(df_weights)[i] <- paste("rep", i, sep = "")
  }

  mean_weights <- rowMeans(df_weights)

  returnObj <- list( mean_weights = mean_weights, df_weights = df_weights, training_instances = training_instances )
  return(returnObj)
}
