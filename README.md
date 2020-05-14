### Installation

Download and install Rtools: https://cran.r-project.org/bin/windows/Rtools/ *(Rtools required to install packages from GitHub)*

```
# install.packages("devtools")
library(devtools)
install_github("johnplloyd/R_packages/PACKAGE_NAME")
# PACKAGE_NAME: lloydUtils, lloydPerf
```

### lloydUtils

Set of basic, custom R functions John often uses while coding in R.

>corner
>
>intersect.iterate
>
>ordered_dot_plot
>
>p_round.display
>
>remove_outliers
>
>transparent_color
>
>write_df

### lloydPerf

Set of functions for calculating performance metrics.

Functions included:
>calc_AUCROC
>
>calc_MCC
>
>calc_median_error
>
>calc_MSE
>
>calc_RSS
>
>performance.regression
