% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ordered_dot_plot.R
\name{ordered_dot_plot}
\alias{ordered_dot_plot}
\title{Generates an ordered dot plot.}
\usage{
ordered_dot_plot(
  x,
  ylim = "",
  xlab = NA,
  ylab = NA,
  main = NA,
  spread = 0.3,
  las = 1,
  iqr_line = TRUE,
  median_line = TRUE,
  outlier = TRUE,
  ordered = TRUE,
  cex = 1,
  dot_col = "black",
  median_col = "red"
)
}
\arguments{
\item{x}{matrix, data.frame, or list of vectors}

\item{spread}{Distance of dot distributions from one another. Smaller spread = closer together, spread > 0.5 will cause overlap of neighboring distributions}

\item{iqr_line}{Boolean indicating whether lines indicating the 25th and 75th percentiles are plotted.}

\item{median_line}{Boolean indicating whether a line indicating the median of a distribtuion is plotted.}

\item{outlier}{Boolean indicating whether outlier values should be included in the plot.}

\item{ordered}{Boolean indicating whether dots should be ordered from high to low or randomly shuffled. ordered = FALSE is similar to a jitter plot.}

\item{file}{Location to save file}
}
\value{
Generates an ordered dot plot.
}
\description{
Vizualise a distribution of data with ordered dots. Function will take a data frame, matrix, or list of vectors as input. Median values are indicated by thick red lines, 1st and 3rd quartiles are indicated with dotted gray lines. ylim, xlab, ylab, main, las, and cex arguments function as in plots in the base installation of R. Example ordered dot at: https://github.com/johnplloyd/R_packages/blob/master/lloydUtils/IC50.ordered_dot_plot.pdf
}
