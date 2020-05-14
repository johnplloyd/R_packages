#' Saves matrix/dataframe to tab-delimited text file.
#'
#' This function saves matrix/dataframe to tab-delimited text file with an ID included for the row names column
#' @param X matrix or data.frame
#' @param rowNm_header Column ID for row names
#' @param file Location to save file
#' @return Writes the matrix/data.frame to text file
#' @export
#' @examples
#' write_df(X = matrix, rowNm_header = "rowType", file = "../path/to/file.txt")

write_df <- function (X, rowNm_header, file) {
  
  X.rowNm <- data.frame(val = row.names(X))
  names(X.rowNm) <- rowNm_header
  
  df.out <- cbind ( X.rowNm, X )
  
  write.table( df.out, file = file, quote = F, sep = "\t", row.names = F )
}