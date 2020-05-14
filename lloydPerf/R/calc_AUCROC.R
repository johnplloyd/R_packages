#' Calculates area under the curve: receiver operating characteristic (AUC-ROC)
#'
#' This function takes a scores vector and a labels vector and calculates AUC-ROC.
#' @param scores vector with prediction scores
#' @param labels vector with class labels. Positive class: 1, negative class: 0
#' @return AUC-ROC value
#' @export
#' @examples
#' s = c(1, 0.8, 0.6, 0.4, 0.2, 0)
#' l = c(1, 1, 0, 1, 0, 0)
#' calc_AUCROC(scores = s, labels = l)
#' [1] 0.8888889

calc_AUCROC <- function(scores, labels){

  if (!requireNamespace("ROCR", quietly = TRUE)) {
    stop("\nPackage \"ROCR\" needed for calc_AUCROC() to work.\nPlease install: install.packages(\"ROCR\")",
         call. = FALSE)
  }

  library(ROCR)
  roc_pred <- prediction(scores, labels) # arg1 = prediction, arg2 = labels
  auc_obj <- performance(roc_pred, "auc")
  auc_roc <- auc_obj@y.values[[1]]
  return(auc_roc)
}
