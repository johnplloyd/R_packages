### Installation

Download and install Rtools: https://cran.r-project.org/bin/windows/Rtools/ *(Rtools required to install packages from GitHub)*

```
# install.packages("devtools")
library(devtools)

install_github("johnplloyd/R_packages/lloydUtils")
library(lloydUtils)

install_github("johnplloyd/R_packages/lloydPerf")
library(lloydPerf)
```

### lloydUtils

Set of general, custom R functions John often uses while coding in R.

Functions included:
```
corner
intersect.iterate
ordered_dot_plot
p_round.display
remove_outliers
transparent_color
write_df
```

### lloydPerf

Set of functions for calculating performance metrics in R.

Functions included:
```
calc_AUCROC (requires package: ROCR)
calc_kappa
calc_MCC
calc_median_error
calc_MSE
calc_RSS
performance.regression
```