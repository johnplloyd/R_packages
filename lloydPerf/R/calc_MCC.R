#' Calculates Matthew's Correlation Coefficient (MSE)
#'
#' This function takes true positive (tp), false positive (fp), false negative (fn), and true negative (tn) counts and calculates MCC.
#' @param tp Count of true positives: Integer
#' @param fp Count of false positives: Integer
#' @param fn Count of false negatives: Integer
#' @param tn Count of true negatives: Integer
#' @return MCC value
#' @export
#' @examples
#' calc_MCC(tp = 20, fp = 10, fn = 5, tn = 30)

calc_MCC <- function(tp, fp, fn, tn){
  numer <- (tp*tn) - (fp*fn)
  denom_unSqrt <- (tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)
  denom <- sqrt(denom_unSqrt)
  MCC <- numer/denom
  return(MCC)
}
