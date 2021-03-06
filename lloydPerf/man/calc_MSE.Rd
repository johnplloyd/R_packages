% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_MSE.R
\name{calc_MSE}
\alias{calc_MSE}
\title{Calculates mean squared error (MSE) between two vectors}
\usage{
calc_MSE(x1, x2)
}
\arguments{
\item{x1}{numeric vector 1}

\item{x2}{numeric vector 2}
}
\value{
MSE value
}
\description{
This function takes two vectors and calculates their mean squared error. Vectors must be of equal length. Expected usage is that one vector is observed values and the second vector is predicted values.
}
\examples{
x <- runif(5)
y <- runif(5)
calc_MSE(x1 = x, x2 = y)
[1] 0.7402145
}
