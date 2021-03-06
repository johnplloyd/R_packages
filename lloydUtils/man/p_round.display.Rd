% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/p_round.display.R
\name{p_round.display}
\alias{p_round.display}
\title{Rounds P-values cleanly for display in figures.}
\usage{
p_round.display(x, expression = FALSE)
}
\arguments{
\item{x}{P-values (numeric)}

\item{expression}{Boolean indicating whether the rounded P-value should be returned as an expression.}
}
\value{
Rounded P-value in string format.
}
\description{
This function takes a P-values and generates a clean, rounded version for display in plots/figures. The expression argument is included, and if set to TRUE will allow exponents to be properly displayed as a superscript in the initial plots. However, 'expression' text behaves poorly if the figure is saved in PDF format and opened in vector graphics software.
}
\examples{
p_round.display(x = 1.234e-56, expression = F)
}
