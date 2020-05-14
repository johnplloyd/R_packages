#' Converts a named color to a transparent version.
#'
#' This function takes a named color and reduced the opacity based on the alpha argument.
#' @param color_name color name recognized by R
#' @param alpha alpha level: value between 0 and 1, smaller values indicate more transparency
#' @return RGD code of the color with transparency included
#' @export
#' @examples
#' transparent_color(color_name = "red", alpha = 0.25)

transparent_color <- function(color_name, alpha){
  col.rgb <- col2rgb(color_name)
  col.rgb2 <- col.rgb/255
  col.transparent <- rgb( col.rgb2[1], col.rgb2[2], col.rgb2[3], alpha = alpha )
  return(col.transparent)
}