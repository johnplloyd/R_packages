#' Calculates Cohen's kappa
#'
#' This function takes true positive (tp), false positive (fp), false negative (fn), and true negative (tn) counts and calculates Cohen's kappa.
#' @param tp Count of true positives: Integer
#' @param fp Count of false positives: Integer
#' @param fn Count of false negatives: Integer
#' @param tn Count of true negatives: Integer
#' @return kappa value
#' @export
#' @examples
#' calc_kappa(tp = 20, fp = 10, fn = 5, tn = 30)
#' [1] 0.5301205

calc_kappa <- function(tp, fp, fn, tn){
  # from: https://en.wikipedia.org/wiki/Cohen%27s_kappa#Example
  count_sum <- tp+fp+fn+tn

  po <- (tp+tn) / count_sum

  py <- ((tp+fn)/count_sum) * ((tp+fp)/count_sum)
  pn <- ((fp+tn)/count_sum) * ((fn+tn)/count_sum)
  pe <- py + pn

  kappa <- (po-pe) / (1-pe)

  return(kappa)
}
