#' Train and apply multiple machine learning algorithms
#'
#' This function will train a prediction model and apply it to new data.
#' @param model_type algorithm to use: "glment", "plsr"
#' @param y response vector - training
#' @param X feature matrix - training
#' @param newy response vector - testing (plsr only)
#' @param newx feature matrix - testing
#' @param parameters_obj object with algorithm-specific parameters
#' @return Returns an object with three items: 1) vector with predicted response values for the testing instances ($predicted), 2) reporter items, such as parameters selected ($report_items), and 3) model object ($model)
#' @export

train_and_apply_model <- function(model_type, y, X, newy, newx, parameters_obj){

  if(model_type == "glmnet"){
    modelObj <- train_and_apply_glmnet_model( y_glm = y, X_glm = X, newx_glm = newx, glmnet_parameters_obj = parameters_obj )
  }else if(model_type == "plsr"){
    modelObj <- train_and_apply_plsr_model( y_plsr = y, X_plsr = X, newy_plsr = newy, newx_plsr = newx, plsr_parameters_obj = parameters_obj )
  }
  return(modelObj)
}
