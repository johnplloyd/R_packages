#' Generates an ordered dot plot.
#'
#' Vizualise a distribution of data with ordered dots. Function will take a data frame, matrix, or list of vectors as input. Median values are indicated by thick red lines, 1st and 3rd quartiles are indicated with dotted gray lines. ylim, xlab, ylab, main, las, and cex arguments function as in plots in the base installation of R. Example ordered dot at: https://github.com/johnplloyd/R_packages/blob/master/IC50.ordered_dot_plot.pdf
#' @param x matrix, data.frame, or list of vectors
#' @param spread Distance of dot distributions from one another. Smaller spread = closer together, spread > 0.5 will cause overlap of neighboring distributions
#' @param file Location to save file
#' @param outlier Boolean indicating whether outlier values should be included in the plot.
#' @param median_line Boolean indicating whether a line indicating the median of a distribtuion is plotted.
#' @param iqr_line Boolean indicating whether lines indicating the 25th and 75th percentiles are plotted.
#' @param ordered Boolean indicating whether dots should be ordered from high to low or randomly shuffled. ordered = FALSE is similar to a jitter plot.
#' @return Generates an ordered dot plot.
#' @export

ordered_dot_plot <- function (x, ylim='', xlab=NA, ylab=NA, main=NA, spread=0.3, las = 1, iqr_line=TRUE, median_line=TRUE, outlier=TRUE, ordered = TRUE, cex = 1, dot_col = "black", median_col = "red"){

  # read matrix as data frame
  if( !( is.null(dim(x)) ) ){
    x <- data.frame(x)
  }

  # if lacking, add names to x
  if( is.null( names(x) )){
    names(x) <- 1:length (x)
  }

  # calculate and store median and quantile values
  # calculated prior to removal of outliers, if requested
  if(median_line == TRUE){
    medn_l <- lapply(x, function(x) median(x, na.rm = T) )
  }

  if(iqr_line == TRUE){
    quant_l <- lapply(x, function(x) quantile(x, na.rm = T) )
  }

  # remove outliers
  # resulting vectors may not be of equal length: return as list of vectors
  if(outlier == FALSE){
    x.rm <- list()
    i = 1
    for (nm in names (x)){
      x.sub <- na.omit( x [[nm]] )
      x.sub.rm <- remove_outliers (x.sub)
      x.rm[[i]] <- x.sub.rm
      i <- i + 1
    }
    names(x.rm) <- names(x)
    x <- x.rm
  }

  # calculate y limits as min and max of all data
  if( ylim == '' ){
    y_min <- min (unlist (x), na.rm = T)
    y_max <- max (unlist (x), na.rm = T)
    ylim <- c (y_min, y_max)
  }

  # generate empty plot of appropriate dimensions
  # chart is effectively a scatterplot with custom x-axis
  plot (1, type = "n", xlim = c (1-spread, length(x)+spread), ylim = ylim, xaxt = 'n', xlab=xlab, ylab=ylab, main=main)

  # loop through x and plot data
  # y values: data distribution, sorted low to high
  # x values: even spread across subset of x axis
  for (i in 1:length (names (x))){
    print(i)
    nm <- names (x)[i]

    y_vals <- sort( na.omit(x[[nm]]) )

    if(length(y_vals) == 0){
      pass <- "PASS"
    }else{
      spread_min <- i-spread
      spread_max <- i+spread
      x_vals <- seq (from=spread_min, to = spread_max, length=length(y_vals))

      if(ordered == FALSE){
        if(length(x_vals) > 1){
          x_vals <- sample( x_vals )
        }
      }

      if (iqr_line == TRUE){
        iqr_low <- quant_l[[i]][[2]]
        iqr_high <- quant_l[[i]][[4]]

        segments (x0=spread_min, y0=iqr_low, x1=spread_max, y1=iqr_low, col = "gray40", lty="dotted")
        segments (x0=spread_min, y0=iqr_high, x1=spread_max, y1=iqr_high, col = "gray40", lty="dotted")
      }

      #points (x=x_vals, y=y_vals, pch=16, cex=0.6)
      points (x=x_vals, y=y_vals, pch=16, cex = cex, col = dot_col)

      if (median_line == TRUE){
        medn <- medn_l [[i]]
        segments (x0=spread_min, y0=medn, x1=spread_max, y1=medn, col = median_col, lwd=2)
      }
    }
  }

  # add custom x axis
  axis( 1, at = 1:length(x), labels = names(x), las = las )
}
