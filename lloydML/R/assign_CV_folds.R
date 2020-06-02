#' Assign cross-validation folds
#'
#' This function randomly assigns fold assignments.
#' @param n_instances number of instances to assign CV folds: Integer
#' @param n_fold number of CV folds: Integer, default = 10
#' @return Returns a vector with fold assignments
#' @export
#' @examples
#' assign_CV_folds(n_instances = 10, n_fold = 3)
#' > [1] 1 1 1 3 2 1 3 2 2 3

assign_CV_folds <- function(n_instances, n_fold = 10){

  fold_assignments <- rep( 1:n_fold, ceiling(n_instances/n_fold))[1:n_instances]
  fold_assignments <- sample(fold_assignments)

  return(fold_assignments)
}
