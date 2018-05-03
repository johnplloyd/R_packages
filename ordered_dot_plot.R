
remove_outliers <- function (x){
  quart_low <- quantile (x)[[2]]
  quart_high <- quantile (x)[[4]]
  outlier_dist <- IQR (x) * 1.5
  
  low_thresh <- quart_low - outlier_dist
  high_thresh <- quart_high + outlier_dist
  
  x <- x [ which (x > low_thresh & x < high_thresh) ]
  
  return (x)
}

ordered_dot_plot <- function (x, ylim='', xlab=NA, ylab=NA, spread=0.3, iqr_line=TRUE, median_line=TRUE, outlier=TRUE){
  
  # if lacking, add names to x
  if( is.null (names (x))){
    names(x) <- 1:length (x)
  }
  
  # calculate and store median and quantile values
  # calculated prior to removal of outliers, if requested
  if(median_line == TRUE){
    medn_l <- lapply(x, median)
  }
  
  if(iqr_line == TRUE){
    quant_l <- lapply(x, quantile)
  }
  
  # remove outliers
  # resulting vectors may not be of equal length: return as list of vectors
  if(outlier == FALSE){
    x.rm <- list()
    i = 1
    for (nm in names (x)){
      x.sub <- x [[nm]]
      x.sub.rm <- remove_outliers (x.sub)
      x.rm[[i]] <- x.sub.rm
      i <- i + 1
    }
    names(x.rm) <- names(x)
    x <- x.rm
  }
  
  # calculate y limits as min and max of all data
  if( ylim == '' ){
    y_min <- min (unlist (x))
    y_max <- max (unlist (x))
    ylim <- c (y_min, y_max)
  }
  
  # generate empty plot of appropriate dimensions
  # chart is effectively a scatterplot with custom x-axis
  plot (1, type = "n", xlim = c (1-spread, length(x)+spread), ylim = ylim, xaxt = 'n', xlab=xlab, ylab=ylab)
  
  # loop through x and plot data
  # y values: data distribution, sorted low to high
  # x values: even spread across subset of x axis
  for (i in 1:length (names (x))){
    print(i)
    nm <- names (x)[i]
    
    y_vals <- sort(x[[nm]])
    
    spread_min <- i-spread
    spread_max <- i+spread
    x_vals <- seq (from=spread_min, to = spread_max, length=length(y_vals))
    
    if (iqr_line == TRUE){
      iqr_low <- quant_l[[i]][[2]]
      iqr_high <- quant_l[[i]][[4]]
      
      segments (x0=spread_min, y0=iqr_low, x1=spread_max, y1=iqr_low, col = "gray40", lty="dotted")
      segments (x0=spread_min, y0=iqr_high, x1=spread_max, y1=iqr_high, col = "gray40", lty="dotted")
    }
    
    points (x=x_vals, y=y_vals, pch=16, cex=0.6)
    
    if (median_line == TRUE){
      medn <- medn_l [[i]]
      segments (x0=spread_min, y0=medn, x1=spread_max, y1=medn, col = "red", lwd=2)
    }
  }
  
  # add custom x axis
  axis (1, at=1:length (x), labels=names (x))
}
