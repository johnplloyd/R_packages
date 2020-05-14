### Installation

Download and install Rtools: https://cran.r-project.org/bin/windows/Rtools/ *(Rtools required to install packages from GitHub)*

```
# install.packages("devtools")
library(devtools)
install_github("johnplloyd/R_packages/[pkg_name]")
# pkg_name: lloydUtils, lloydPerf
```

### lloydUtils

Set of custom R functions John often uses while coding in R.

### lloydPerf

Set of functions for calculating performance metrics.

Functions included:
>calc_AUCROC

>calc_MCC

>calc_median_error

>calc_MSE

>calc_RSS

>performance.regression

#### ordered_dot_plot():

Vizualise a distribution of data with ordered dots. Function will take a data frame, matrix, or list of vectors as input. Median values are indicated by thick red lines, 1st and 3rd quartiles are indicated with dotted gray lines. Either line set can be turned off with median_line or iqr_line arguments. Spread indicates distance of dot distributions from one another. Smaller spread = closer together, spread > 0.5 will cause overlap of neighboring distributions. Outliers can be removed from vizualization through use of the oulier argument.

Example plot: IC50.ordered_dot_plot.pdf (data from: Klijn et al., 2015, Nature Biotech)
